module SimpleCalculator (subcommand) where

import LambdaParser.UntypedLambdaParser
import UntypedLambdaCalculus.LambdaReduction (calculateNormalResult)
import BaseException
import CommandArg

import qualified Data.Map as Map
import qualified Data.Set as Set
import GHC.IO.Handle (hSetEncoding, hGetContents)
import GHC.IO.Encoding (utf8)
import GHC.IO.IOMode (IOMode(ReadMode))
import GHC.IO.Handle.FD (openFile)
import Control.Exception (throwIO)


parseCodeFiles :: [String] -> IO ([ValDef])
parseCodeFiles [] = return []
parseCodeFiles (codeFile : codeFiles) = do
    handle <- openFile codeFile ReadMode
    hSetEncoding handle utf8
    codeContent <- hGetContents handle
    valDefList <- case runParseCode codeFile codeContent of
        Right result -> return result
        Left e -> throwIO $ BaseException ("parser error: " ++ show e)
    valDefList' <- parseCodeFiles codeFiles
    return (valDefList ++ valDefList')


subcommand :: [String] -> IO ()
subcommand args = do
    let argList = parseArgs args
    functionName <- case lookup "f" argList of
        Just v -> return v
        Nothing -> return "main"
    let codeFiles = map (\(_, value) -> value) (filter (\(name, _) -> (name == "")) argList)
    valDefList <- parseCodeFiles codeFiles
    let valDefMap = valDefListToMap valDefList
    lambdaTerm <- case Map.lookup functionName valDefMap of
        Just v -> return $ toLambdaTerm Set.empty valDefMap v
        Nothing -> throwIO $ BaseException ("not found " ++ functionName)
    let result = calculateNormalResult lambdaTerm
    print result


