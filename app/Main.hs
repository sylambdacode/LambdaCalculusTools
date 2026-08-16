module Main where

import qualified SimpleCalculator as SimpleCalculator
import qualified SimpleLang as SimpleLang
import qualified SimpleKrivineMachineRunner as SimpleKrivineMachineRunner
import BaseException

import System.Environment(getArgs)
import GHC.IO.Handle.FD (stderr)
import Control.Exception (throw, try)
import GHC.IO.Handle.Text (hPutStrLn)


mainHandler :: IO ()
mainHandler = do
    args <- getArgs
    mode <- if length args < 1
        then throw $ BaseException "no mode"
        else return (args !! 0)
    case mode of
        "runKrivineMachine" -> do
            SimpleKrivineMachineRunner.subcommand (drop 1 args)
        "calculate" -> do
            SimpleCalculator.subcommand (drop 1 args)
        "simplelang" -> do
            SimpleLang.subcommand (drop 1 args)
        _ -> throw $ BaseException "unknown mode"

main :: IO ()
main = do
    result <- try mainHandler
    case result of
        Left e -> hPutStrLn stderr (show (e :: BaseException))
        Right v -> return v
