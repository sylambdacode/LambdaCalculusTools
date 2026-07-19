module SimpleLang (subcommand) where

import UntypedLambdaCalculus.LambdaTerm
import LambdaParser

import BaseException

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.IO.Handle (hSetEncoding, hGetContents)
import GHC.IO.Encoding (utf8)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import UntypedLambdaCalculus.LambdaReduction (calculateWeakNormalHeadResult)
import Control.Exception (throw)

toSimpleLangString :: String -> String
toSimpleLangString str = ('\'' : str)

fromSimpleLangString :: String -> String
fromSimpleLangString ('\'' : str) = str
fromSimpleLangString _ = error "not string"

toSimpleLangInt :: Integer -> String
toSimpleLangInt i = show i

fromSimpleLangInt :: String -> Integer
fromSimpleLangInt i = read i

toSimpleLangBool :: Bool -> String
toSimpleLangBool True = "true"
toSimpleLangBool False = "false"

fromSimpleLangBool :: String -> Bool
fromSimpleLangBool "true" = True
fromSimpleLangBool "false" = False
fromSimpleLangBool _ = error "not bool"

matchFunction :: LambdaTerm -> IO String

matchFunction (Application (Application (Variable "concat")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return (toSimpleLangString (fromSimpleLangString arg1val ++ fromSimpleLangString arg2val))

matchFunction (Application (Application (Variable "print")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    putStr (fromSimpleLangString arg1val)
    evalExpr (Application arg2 (Variable "'"))

matchFunction (Application (Variable "readline")  arg1) = do
    line <- getLine
    evalExpr (Application arg1 (Variable (toSimpleLangString line)))

matchFunction (Application (Application (Variable "strict")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    evalExpr (Application arg2 (Variable arg1val))

matchFunction (Application (Variable "inttostring")  arg1) = do
    arg1val <- evalExpr arg1
    return (toSimpleLangString arg1val)

matchFunction (Application (Variable "stringtoint")  arg1) = do
    arg1val <- evalExpr arg1
    return (fromSimpleLangString arg1val)

matchFunction (Application (Variable "stringlength")  arg1) = do
    arg1val <- evalExpr arg1
    let v = length (fromSimpleLangString arg1val)
    return (toSimpleLangInt (toInteger v))

matchFunction (Application (Application (Application (Variable "substring")  arg1) arg2) arg3) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    arg3SimpleLangValue <- evalExpr arg3
    let arg1val = fromSimpleLangString arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    let arg3val = fromSimpleLangInt arg3SimpleLangValue
    let arg2valInt = fromInteger (arg2val)
    let arg3valInt = fromInteger (arg3val)
    return $ toSimpleLangString (take arg3valInt (drop arg2valInt arg1val))

matchFunction (Application (Application (Variable "add")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val + arg2val)

matchFunction (Application (Application (Variable "sub")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val - arg2val)

matchFunction (Application (Application (Variable "mul")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val * arg2val)

matchFunction (Application (Application (Variable "div")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangInt (arg1val `div` arg2val)

matchFunction (Application (Application (Variable "eq")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return $ toSimpleLangBool (arg1val == arg2val)

matchFunction (Application (Application (Variable "neq")  arg1) arg2) = do
    arg1val <- evalExpr arg1
    arg2val <- evalExpr arg2
    return $ toSimpleLangBool (arg1val /= arg2val)

matchFunction (Application (Application (Variable "lt")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val < arg2val)

matchFunction (Application (Application (Variable "gt")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val > arg2val)

matchFunction (Application (Application (Variable "lteq")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val <= arg2val)

matchFunction (Application (Application (Variable "gteq")  arg1) arg2) = do
    arg1SimpleLangValue <- evalExpr arg1
    arg2SimpleLangValue <- evalExpr arg2
    let arg1val = fromSimpleLangInt arg1SimpleLangValue
    let arg2val = fromSimpleLangInt arg2SimpleLangValue
    return $ toSimpleLangBool (arg1val >= arg2val)

matchFunction (Application (Application (Application (Variable "if")  arg1) arg2) arg3) = do
    arg1val <- evalExpr arg1
    if fromSimpleLangBool arg1val
        then do
            evalExpr arg2
        else do
            evalExpr arg3


matchFunction (Variable a) = return a
matchFunction _ = error "match function error"

evalExpr :: LambdaTerm -> IO String
evalExpr lambdaTerm = do
    let l = calculateWeakNormalHeadResult lambdaTerm
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
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throw $ BaseException "not found main"
    print lambdaTerm
    _ <- evalExpr (readLambdaTerm (show lambdaTerm))
    return ()


