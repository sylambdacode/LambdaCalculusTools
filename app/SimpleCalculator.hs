{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module SimpleCalculator (subcommand) where

import LambdaParser.UntypedLambdaParser
import UntypedLambdaCalculus.LambdaTerm (LambdaTerm (Variable, Application, Abstraction))
import UntypedLambdaCalculus.LambdaReduction (calculateNormalResult, calculateWeakNormalHeadResult, isWeakNormalHeadForm)
import CommandException
import CommandArg

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (openFile, hSetEncoding, hGetContents, utf8, IOMode(ReadMode))
import Control.Exception (throw, throwIO)


matchFunction :: LambdaTerm -> IO LambdaTerm

matchFunction (Application (Application (Variable "strict")  arg1) arg2) = do
    arg1Value <- evalLambdaTerm arg1
    return (Application arg2 arg1Value)

matchFunction (Application (Application (Variable "strict-normal")  arg1) arg2) = do
    let arg1NormalResult = calculateNormalResult arg1
    return (Application arg2 arg1NormalResult)

matchFunction (Application (Variable "_print-bool")  arg1) = do
    arg1Value <- evalLambdaTerm arg1
    let arg1BoolValue = case arg1Value of
            Abstraction x (Abstraction y (Variable v)) -> if v == x || v == y then x == v else error "not bool"
            _ -> error "not bool"
    putStrLn (if arg1BoolValue then "true" else "false")
    return arg1

matchFunction (Application (Variable "_print-lambda-term")  arg1) = do
    print arg1
    return arg1

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
    printResult <- case lookup "p" argList of
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

