module SimpleLang (subcommand) where

import UntypedLambdaCalculus.LambdaTerm
import LambdaParser

import qualified Data.Map as Map
import GHC.IO.Handle (hSetEncoding, hGetContents)
import GHC.IO.Encoding (utf8)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import UntypedLambdaCalculus.LambdaReduction (calculateWeakNormalHeadResult)
import Control.Exception (Exception, throw)
data BaseException = BaseException String

instance Exception BaseException

instance Show BaseException where
    show (BaseException message) = message


matchFunction :: LambdaTerm -> IO String
matchFunction (Application (Application (Variable "concat")  arg1) arg2) = do
    ('\'' : arg1val) <- evalExpr arg1
    ('\'' : arg2val) <- evalExpr arg2
    return ('\'' : (arg1val ++ arg2val))
matchFunction (Application (Application (Variable "print")  arg1) arg2) = do
    ('\'' : arg1val) <- evalExpr arg1
    putStrLn arg1val
    evalExpr (Application arg2 (Variable "'"))
matchFunction (Application (Variable "tostring")  arg1) = do
    arg1val <- evalExpr arg1
    return ('\'' : arg1val)

matchFunction (Application (Application (Variable "add")  arg1) arg2) = do
    arg1strval <- evalExpr arg1
    arg2strval <- evalExpr arg2
    let arg1val = (read arg1strval) :: Integer
    let arg2val = (read arg2strval) :: Integer
    return $ show (arg1val + arg2val)
matchFunction (Application (Application (Variable "sub")  arg1) arg2) = do
    arg1strval <- evalExpr arg1
    arg2strval <- evalExpr arg2
    let arg1val = (read arg1strval) :: Integer
    let arg2val = (read arg2strval) :: Integer
    return $ show (arg1val - arg2val)

matchFunction (Application (Application (Variable "mul")  arg1) arg2) = do
    arg1strval <- evalExpr arg1
    arg2strval <- evalExpr arg2
    let arg1val = (read arg1strval) :: Integer
    let arg2val = (read arg2strval) :: Integer
    return $ show (arg1val * arg2val)

matchFunction (Application (Application (Variable "div")  arg1) arg2) = do
    arg1strval <- evalExpr arg1
    arg2strval <- evalExpr arg2
    let arg1val = (read arg1strval) :: Integer
    let arg2val = (read arg2strval) :: Integer
    return $ show (arg1val `div` arg2val)
matchFunction (Application (Application (Variable "eq")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return $ show (arg1val == arg2val)
matchFunction (Application (Application (Variable "neq")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return $ show (arg1val /= arg2val)
matchFunction (Application (Application (Variable "lt")  arg1) arg2) = do
    arg1strval <- evalExpr arg1
    arg2strval <- evalExpr arg2
    let arg1val = (read arg1strval) :: Integer
    let arg2val = (read arg2strval) :: Integer
    return $ show (arg1val < arg2val)
matchFunction (Application (Application (Variable "gt")  arg1) arg2) = do
    arg1strval <- evalExpr arg1
    arg2strval <- evalExpr arg2
    let arg1val = (read arg1strval) :: Integer
    let arg2val = (read arg2strval) :: Integer
    return $ show (arg1val > arg2val)


matchFunction (Application (Application (Application (Variable "if")  arg1) arg2) arg3) = do
    arg1val <- evalExpr arg1
    --putStrLn ("if if if : " ++ arg1val)
    if read arg1val
        then do
            --putStrLn ("if then: " ++ show arg2)
            evalExpr arg2
        else do
            --putStrLn ("if else: " ++ show arg3)
            evalExpr arg3


matchFunction (Variable a) = return a
matchFunction _ = error "match function error"

evalExpr :: LambdaTerm -> IO String
evalExpr lambdaTerm = do
    let l = calculateWeakNormalHeadResult lambdaTerm
    --print v
    --putStrLn ("-----" ++ (show lambdaTerm))
    --putStrLn ("=====" ++ (show l))
    --threadDelay 1000000
    v <- matchFunction l
    return v

subcommand :: String -> String -> IO ()
subcommand functionName codeFile = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefMap <- case parseCode "(code)" codeContent of
        Right result -> return $ valDefListToMap result
        Left e -> throw $ BaseException ("parser error: " ++ show e)
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm valDefMap v
        Nothing -> throw $ BaseException "not found main"

    --print lambdaTerm
    result <- evalExpr lambdaTerm
    print result
    return ()


