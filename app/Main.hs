module Main where

import qualified SimpleCalculator as SimpleCalculator
import qualified SimpleLang as SimpleLang
import qualified SimpleKrivineMachineRunner as SimpleKrivineMachineRunner

import System.Environment(getArgs)
import GHC.IO.Handle.FD (stderr)
import Control.Exception (Exception, throw, catch)
import GHC.IO.Handle.Text (hPutStrLn)

data BaseException = BaseException String

instance Exception BaseException

instance Show BaseException where
    show (BaseException message) = message

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
            SimpleKrivineMachineRunner.subcommand codeFile
        "calculate" -> do
            functionName <- if length args < 2
                then throw $ BaseException "no function name"
                else return (args !! 1)
            codeFile <- if length args < 3
                then throw $ BaseException "no code file"
                else return (args !! 2)
            SimpleCalculator.subcommand functionName codeFile
        "simplelang" -> do
            functionName <- if length args < 2
                then throw $ BaseException "no function name"
                else return (args !! 1)
            codeFile <- if length args < 3
                then throw $ BaseException "no code file"
                else return (args !! 2)
            SimpleLang.subcommand functionName codeFile

        _ -> throw $ BaseException "unknown mode"

main :: IO ()
main = catch mainHandler (\e -> hPutStrLn stderr (show (e :: BaseException)))
