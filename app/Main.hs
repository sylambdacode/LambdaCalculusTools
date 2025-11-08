module Main where

import DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified DeBruijnLambdaTerm

import LambdaParser

import qualified Data.Map as Map

import KrivineMachine


import LambdaTermTools

import GHC.TopHandler (flushStdHandles)
import Control.Monad.State (StateT (runStateT), MonadIO (liftIO), MonadState (get, put))
import GHC.IO.Handle (isEOF, hSetEncoding, hGetContents)
import System.Environment(getArgs)
import GHC.IO.Encoding (utf8)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile, stderr)
import LambdaReduction (calculateNormalResult)
import Control.Exception (Exception, throw, catch)
import GHC.IO.Handle.Text (hPutStrLn)

data BaseException = BaseException String

instance Exception BaseException

instance Show BaseException where
    show (BaseException message) = message

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

transWithIO _ n = do
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
            '1' -> return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 2))
            '0' -> return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 1))
            _ -> error "Error"
        return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Application (DeBruijnLambdaTerm.Application (DeBruijnLambdaTerm.Variable 1) value) (DeBruijnLambdaTerm.Variable ((-3) - newInputCount)))
    else return $ DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Abstraction (DeBruijnLambdaTerm.Variable 1))


runMode :: String -> IO ()
runMode codeFile = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefMap <- case parseCode "(code)" codeContent of
        Right result -> return $ valDefListToMap result
        Left e -> throw $ BaseException ("parser error: " ++ show e)
    mainLambdaTerm <- case Map.lookup "P___" valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> throw $ BaseException "not found main"
    ioWrapper <- case Map.lookup "ioWrapper___" valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> throw $ BaseException "not found ioWrapper___"
    
    let globalFreeVariableMap = Map.fromList [("O0___", -1), ("O1___", -2), ("input___", -3)]
    let ioWrapperDeBruijnLambdaTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] ioWrapper
    let deBruijnLambadTerm = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap [Map.empty] mainLambdaTerm
    let r = krivineMachine (transWithIO ioWrapperDeBruijnLambdaTerm) deBruijnLambadTerm (Environment []) (Environment [])
    _ <- runStateT r []
    return ()

compileMode :: String -> String -> IO ()
compileMode functionName codeFile = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefMap <- case parseCode "(code)" codeContent of
        Right result -> return $ valDefListToMap result
        Left e -> throw $ BaseException ("parser error: " ++ show e)
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> throw $ BaseException "not found main"
    let result = calculateNormalResult lambdaTerm
    print result

mainHandler :: IO ()
mainHandler = do
    args <- getArgs
    mode <- if length args < 1
        then throw $ BaseException "no mode"
        else return (args !! 0)
    case mode of
        "run" -> do
            codeFile <- if length args < 2
                then throw $ BaseException "no code file"
                else return (args !! 1)
            runMode codeFile
        "compile" -> do
            functionName <- if length args < 2
                then throw $ BaseException "no function name"
                else return (args !! 1)
            codeFile <- if length args < 3
                then throw $ BaseException "no code file"
                else return (args !! 2)
            compileMode functionName codeFile
        _ -> throw $ BaseException "unknown mode"

main :: IO ()
main = catch mainHandler (\e -> hPutStrLn stderr (show (e :: BaseException)))