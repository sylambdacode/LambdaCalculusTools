{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaCalculusTools.CommandLineInterface.SimpleCalculatorSubcommand (subcommand) where

import LambdaCalculusTools.LambdaParser.UntypedLambdaParser
import LambdaCalculusTools.UntypedLambdaCalculus.LambdaReduction (calculateNormalResult)
import LambdaCalculusTools.CommandLineInterface.CommandException
import LambdaCalculusTools.CommandLineInterface.CommandArg
import LambdaCalculusTools.LambdaExtension.LambdaExtensionCalculator

import qualified Data.Map as Map
import System.IO (openFile, hSetEncoding, hGetContents, utf8, IOMode(ReadMode))
import Control.Exception (throw, throwIO)
import Control.Monad.State (StateT (runStateT))


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
    let lambdaTermMap = valDefListToLambdaTermMap valDefList
    lambdaTerm <- case Map.lookup functionName lambdaTermMap of
        Just v -> return v
        Nothing -> throwIO $ CommandException ("not found " ++ functionName)
    case calculateType of
        "type1" -> do
            let result = calculateNormalResult lambdaTerm
            if printResult == "enable" then print result else return ()
        "type2" -> do
            (result, _) <- runStateT (evalLambdaTerm return lambdaTerm) ()
            if printResult == "enable" then print result else return ()
        unknownType -> throw $ CommandException ("unknown calculate type: " ++ unknownType)

