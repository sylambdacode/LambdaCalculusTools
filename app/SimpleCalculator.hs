{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module SimpleCalculator (subcommand) where

import LambdaParser.UntypedLambdaParser
import UntypedLambdaCalculus.LambdaTerm (LambdaTerm (Variable, Application, Abstraction), alphaEq)
import UntypedLambdaCalculus.LambdaReduction (calculateNormalResult, calculateWeakNormalHeadResult, isWeakNormalHeadForm)
import CommandException
import CommandArg
import SimpleCalculatorException

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (openFile, hSetEncoding, hGetContents, utf8, IOMode(ReadMode))
import Control.Exception (throw, throwIO)
import Data.Char (chr, ord)


toSimpleValueString :: String -> LambdaTerm
toSimpleValueString str = Variable ('\'' : str)

fromSimpleValueString :: LambdaTerm -> String
fromSimpleValueString (Variable ('\'' : str)) = str
fromSimpleValueString lambdaTerm = throw (SimpleCalculatorException ("not string: " ++ show lambdaTerm))

toSimpleValueInt :: Integer -> LambdaTerm
toSimpleValueInt i = Variable ('_' : (show i))

fromSimpleValueInt :: LambdaTerm -> Integer
fromSimpleValueInt (Variable ('_' : i)) = read i
fromSimpleValueInt lambdaTerm = throw (SimpleCalculatorException ("not int: " ++ show lambdaTerm))

toSimpleValueBool :: Bool -> LambdaTerm
toSimpleValueBool True = Variable "_true"
toSimpleValueBool False = Variable "_false"

fromSimpleValueBool :: LambdaTerm -> Bool
fromSimpleValueBool (Variable "_true") = True
fromSimpleValueBool (Variable "_false") = False
fromSimpleValueBool lambdaTerm = throw (SimpleCalculatorException ("not bool: " ++ show lambdaTerm))


matchFunction :: LambdaTerm -> IO LambdaTerm

matchFunction (Application (Application (Variable "_strict") arg1) arg2) = do
    arg1Value <- evalLambdaTerm arg1
    return (Application arg2 arg1Value)

matchFunction (Application (Application (Variable "_strict-normal") arg1) arg2) = do
    let arg1NormalResult = calculateNormalResult arg1
    return (Application arg2 arg1NormalResult)

matchFunction (Application (Variable "_print-bool") arg1) = do
    arg1Value <- evalLambdaTerm arg1
    let arg1BoolValue = case arg1Value of
            Abstraction x (Abstraction y (Variable v)) -> if v == x || v == y then x == v else error "not bool"
            _ -> error "not bool"
    putStrLn (if arg1BoolValue then "true" else "false")
    return arg1

matchFunction (Application (Variable "_print-lambda-term") arg1) = do
    print arg1
    return arg1

matchFunction (Application (Application (Variable "_string-concat")  arg1) arg2) = do
    arg1val <- evalLambdaTerm arg1
    arg2val <- evalLambdaTerm arg2
    return (toSimpleValueString (fromSimpleValueString arg1val ++ fromSimpleValueString arg2val))

matchFunction (Application (Variable "_print") arg1) = do
    arg1val <- evalLambdaTerm arg1
    putStr (fromSimpleValueString arg1val)
    return arg1

matchFunction (Application (Variable "_int-to-string") arg1) = do
    arg1val <- evalLambdaTerm arg1
    let arg1IntegerVal = fromSimpleValueInt arg1val
    return (toSimpleValueString (show arg1IntegerVal))


matchFunction (Application (Variable "_string-to-int") arg1) = do
    arg1val <- evalLambdaTerm arg1
    let arg1StringVal = fromSimpleValueString arg1val
    return (toSimpleValueInt (read arg1StringVal))

matchFunction (Application (Variable "_int-to-char") arg1) = do
    arg1val <- evalLambdaTerm arg1
    let arg1charval = chr (fromInteger (fromSimpleValueInt arg1val))
    return (toSimpleValueString [arg1charval])

matchFunction (Application (Variable "_char-to-int") arg1) = do
    arg1val <- evalLambdaTerm arg1
    let arg1stringval = fromSimpleValueString arg1val
    case arg1stringval of
        c : "" -> return (toSimpleValueInt (toInteger (ord c)))
        "" -> throw (SimpleCalculatorException "char (string) length must be 1")
        _ -> throw (SimpleCalculatorException "char (string) length must be 1")

matchFunction (Application (Variable "_string-length") arg1) = do
    arg1val <- evalLambdaTerm arg1
    let v = length (fromSimpleValueString arg1val)
    return (toSimpleValueInt (toInteger v))

matchFunction (Application (Application (Application (Variable "_string-substring") arg1) arg2) arg3) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    arg3SimpleValueValue <- evalLambdaTerm arg3
    let arg1val = fromSimpleValueString arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    let arg3val = fromSimpleValueInt arg3SimpleValueValue
    let arg2valInt = fromInteger (arg2val)
    let arg3valInt = fromInteger (arg3val)
    return $ toSimpleValueString (take arg3valInt (drop arg2valInt arg1val))

matchFunction (Application (Application (Variable "_int-add") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val + arg2val)

matchFunction (Application (Application (Variable "_int-sub") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val - arg2val)

matchFunction (Application (Application (Variable "_int-mul") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val * arg2val)

matchFunction (Application (Application (Variable "_int-div") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueInt (arg1val `div` arg2val)

matchFunction (Application (Application (Variable "_eq") arg1) arg2) = do
    arg1val <- evalLambdaTerm arg1
    arg2val <- evalLambdaTerm arg2
    return $ toSimpleValueBool (alphaEq Map.empty arg1val arg2val)

matchFunction (Application (Application (Variable "_neq") arg1) arg2) = do
    arg1val <- evalLambdaTerm arg1
    arg2val <- evalLambdaTerm arg2
    return $ toSimpleValueBool (not (alphaEq Map.empty arg1val arg2val))

matchFunction (Application (Application (Variable "_int-eq") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val < arg2val)

matchFunction (Application (Application (Variable "_int-gt") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val > arg2val)

matchFunction (Application (Application (Variable "_int-lt-eq") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val <= arg2val)

matchFunction (Application (Application (Variable "_int-gt-eq") arg1) arg2) = do
    arg1SimpleValueValue <- evalLambdaTerm arg1
    arg2SimpleValueValue <- evalLambdaTerm arg2
    let arg1val = fromSimpleValueInt arg1SimpleValueValue
    let arg2val = fromSimpleValueInt arg2SimpleValueValue
    return $ toSimpleValueBool (arg1val >= arg2val)

matchFunction (Application (Application (Application (Variable "_if") arg1) arg2) arg3) = do
    arg1val <- evalLambdaTerm arg1
    if fromSimpleValueBool arg1val
        then do
            evalLambdaTerm arg2
        else do
            evalLambdaTerm arg3

matchFunction lambdaTerm = return lambdaTerm


evalLambdaTerm :: LambdaTerm -> IO LambdaTerm

evalLambdaTerm lambdaTerm = do
    result <- matchFunction (calculateWeakNormalHeadResult lambdaTerm)
    if isWeakNormalHeadForm result
           then return result
           else evalLambdaTerm result


parseCodeFiles :: [String] -> IO ([ValDef])
parseCodeFiles [] = return []
parseCodeFiles (codeFile : codeFiles) = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefList <- case runParseCode codeFile codeContent of
        Right result -> return result
        Left e -> throwIO $ CommandException ("parser error: " ++ show e)
    valDefList' <- parseCodeFiles codeFiles
    return (valDefList ++ valDefList')


subcommand :: [String] -> IO ()
subcommand args = do
    let argList = parseArgs args
    functionName <- case lookup "f" argList of
        Just v -> return v
        Nothing -> return "main"
    calculateType <- case lookup "t" argList of
        Just v -> return v
        Nothing -> return "type1"
    printResult <- case lookup "r" argList of
        Just v -> return v
        Nothing -> return "enable"

    let codeFiles = map (\(_, value) -> value) (filter (\(name, _) -> (name == "")) argList)
    valDefList <- parseCodeFiles codeFiles
    let valDefMap = valDefListToMap valDefList
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throwIO $ CommandException ("not found " ++ functionName)
    case calculateType of
        "type1" -> do
            let result = calculateNormalResult lambdaTerm
            if printResult == "enable" then print result else return ()
        "type2" -> do
            result <- evalLambdaTerm lambdaTerm
            if printResult == "enable" then print result else return ()
        unknownType -> throw $ CommandException ("unknown calculate type: " ++ unknownType)

