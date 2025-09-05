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




transWithIO :: DeBruijnLambdaTerm -> Int -> IO DeBruijnLambdaTerm
transWithIO wrapper (-1) = do
    putStrLn "0"
    flushStdHandles
    return wrapper

transWithIO wrapper (-2) = do
    putStrLn "1"
    flushStdHandles
    return wrapper

transWithIO _ _ = error "Error"


main :: IO ()
main = do
    testCode <- getContents
    valDefMap <- case parseCode "(test)" testCode of
        Right result -> return $ valDefListToMap result
        Left e -> fail ("parser error: " ++ show e)
    mainLambdaTerm <- case Map.lookup "main" valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> fail "not founf main"
    ioWrapper <- case Map.lookup "ioWrapper___" valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> fail "not found ioWrapper___"
    
    let globalFreeVariableMap = Map.fromList [("O0___", -1), ("O1___", -2)]
    let ioWrapperDeBruijnLambdaTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] ioWrapper
    let deBruijnLambadTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] mainLambdaTerm
    putStrLn $ show $ mainLambdaTerm
    putStrLn $ show $ deBruijnLambadTerm
    print ioWrapperDeBruijnLambdaTerm
    r <- krivineMachine (transWithIO ioWrapperDeBruijnLambdaTerm) deBruijnLambadTerm (Environment []) (Environment [])
    return ()
