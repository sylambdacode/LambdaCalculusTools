{-# LANGUAGE MultilineStrings #-}

module Main where

import LambdaTerm(LambdaTerm)
import qualified LambdaTerm as LambdaTerm
import DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified DeBruijnLambdaTerm as DeBruijnLambdaTerm

import LambdaReduction

import LambdaParser

import Data.Map (Map)
import qualified Data.Map as Map

import KrivineMachine


import LambdaTermTools
import GHC.TopHandler (flushStdHandles)
import Control.Monad.State (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))
import GHC.IO.Handle (isEOF)


get1Or0Char :: IO Char
get1Or0Char = do
    iseof <- isEOF
    if iseof
        then return 'e'
        else do
            c <- getChar
            if c == '0' || c == '1'
            then return c
            else get1Or0Char

transWithIO :: DeBruijnLambdaTerm -> Int -> StateT [Char] IO DeBruijnLambdaTerm
transWithIO wrapper (-1) = do
    liftIO $ putChar '0'
    liftIO flushStdHandles
    return wrapper

transWithIO wrapper (-2) = do
    liftIO $ putChar '1'
    liftIO flushStdHandles
    return wrapper

transWithIO wrapper n = do
    let inputCount = abs n - 3
    let newInputCount = inputCount + 1
    charList <- get
    inputChar <-
        if length charList <= inputCount
            then do
                c <- liftIO get1Or0Char
                put (c : charList)
                return c
            else do
                let c = charList !! (length charList - inputCount - 1)
                return c
    if inputChar == '1' || inputChar == '0'
    then do
        value <- case inputChar of
            '1' -> return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 1))
            '0' -> return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 2))
            _ -> error "Error"
        return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Application (DeBruijnLambdaTerm.Application (DeBruijnLambdaTerm.Variable 1) value) (DeBruijnLambdaTerm.Variable ((-3) - newInputCount)))
    else return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 2))

transWithIO _ _ = error "Error"


main :: IO ()
main = do
    testCode <- readFile "test.lam"
    valDefMap <- case parseCode "(test)" testCode of
        Right result -> return $ valDefListToMap result
        Left e -> fail ("parser error: " ++ show e)
    mainLambdaTerm <- case Map.lookup "main" valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> fail "not founf main"
    ioWrapper <- case Map.lookup "ioWrapper___" valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> fail "not found ioWrapper___"
    
    let globalFreeVariableMap = Map.fromList [("O0___", -1), ("O1___", -2), ("input___", -3)]
    let ioWrapperDeBruijnLambdaTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] ioWrapper
    let deBruijnLambadTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] mainLambdaTerm
    let r = krivineMachine (transWithIO ioWrapperDeBruijnLambdaTerm) deBruijnLambadTerm (Environment []) (Environment [])
    runStateT r []
    return ()
