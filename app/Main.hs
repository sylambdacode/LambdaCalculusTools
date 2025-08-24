{-# LANGUAGE MultilineStrings #-}

module Main where

import LambdaTerm
import LambdaReduction

import LambdaParser

import Data.Map (Map)
import qualified Data.Map as Map

import OtherTools

main :: IO ()
main = do
    testCode <- getContents
    case parseCode "(test)" testCode of
        Right result -> let valDefMap = valDefListToMap result
            in case (Map.lookup "main" valDefMap) of
                Just v -> do
                              putStrLn "开始运行"
                              putStrLn $ show $ toLambdaTerm valDefMap v
                              putStrLn "---------------------------------------------------------------------------------"
                              print $ toBoolList $ calculateNormalResult $ toLambdaTerm valDefMap v
                Nothing -> print "not found main"
        Left e -> do
            putStrLn "parser error:"
            putStrLn $ show e
