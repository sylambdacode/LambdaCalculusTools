{-# LANGUAGE MultilineStrings #-}

module Main where

import LambdaTerm
import LambdaReduction

import LambdaParser

import Data.Map (Map)
import qualified Data.Map as Map

testCode :: String
testCode = """
        plus = λm n f x.m f (n f x);
        3 = (λf x.f (f (f x)));
        2 = (λf x.f (f x));

        {- plus 2 3 = λf x.f (f (f (f (f x)))) -}
        main = plus 2 3;
    """

main :: IO ()
main = case parseCode "(test)" testCode of
    Right result -> let valDefMap = valDefListToMap result
        in case (Map.lookup "main" valDefMap) of
            Just v -> print $ calculateNormalResult $ toLambdaTerm valDefMap v
            Nothing -> print "not found main"
    Left e -> do
        putStrLn "parser error:"
        putStrLn $ show e
