{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module SimpleCalculator (subcommand) where

import LambdaParser.UntypedLambdaParser
import UntypedLambdaCalculus.LambdaTerm (LambdaTerm (Variable, Application))
import UntypedLambdaCalculus.LambdaReduction (calculateNormalResult, calculateWeakNormalHeadResult, isWeakNormalHeadForm)
import CommandException
import CommandArg

import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (openFile, hSetEncoding, hGetContents, utf8, IOMode(ReadMode))
import Control.Exception (throw, throwIO)


matchFunction :: LambdaTerm -> LambdaTerm

matchFunction (Application (Application (Variable "strict")  arg1) arg2) =
    let arg1Value = evalLambdaTerm arg1
    in (Application arg2 arg1Value)

matchFunction (Application (Application (Variable "strict-normal")  arg1) arg2) =
    let arg1NormalResult = calculateNormalResult arg1
    in (Application arg2 arg1NormalResult)

matchFunction lambdaTerm = lambdaTerm


evalLambdaTerm :: LambdaTerm -> LambdaTerm

evalLambdaTerm lambdaTerm =
    let result = matchFunction (calculateWeakNormalHeadResult lambdaTerm)
    in if isWeakNormalHeadForm result
           then result
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

    let codeFiles = map (\(_, value) -> value) (filter (\(name, _) -> (name == "")) argList)
    valDefList <- parseCodeFiles codeFiles
    let valDefMap = valDefListToMap valDefList
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throwIO $ CommandException ("not found " ++ functionName)
    let result = case calculateType of
                     "type1" -> calculateNormalResult lambdaTerm
                     "type2" -> evalLambdaTerm lambdaTerm
                     unknownType -> throw $ CommandException ("unknown calculate type: " ++ unknownType)
    print result


