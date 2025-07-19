module Main where

import LambdaTerm
import LambdaReduction

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map



lambdaTermList :: [LambdaTerm] -> LambdaTerm

lambdaTermList (lambdaTerm : otherLambdaTermList) =
    foldl (\l r -> Application l r) lambdaTerm otherLambdaTermList


lambdaFunction :: [String] -> LambdaTerm -> LambdaTerm
lambdaFunction (variableName : []) bodyLambdaTerm =
    Abstraction variableName bodyLambdaTerm
lambdaFunction (variableName : otherVariableNameList) bodyLambdaTerm =
    Abstraction variableName (lambdaFunction otherVariableNameList bodyLambdaTerm)

lambdaTermOfNumber3 = lambdaFunction ["f", "x"] (lambdaTermList [Variable "f", lambdaTermList [Variable "f", lambdaTermList [Variable "f", lambdaTermList [Variable "x"]]]])


lambdaTermOfNumber27 = lambdaTermList [lambdaTermOfNumber3, lambdaTermOfNumber3]

getResult lambdaTerm =
    if alphaEq Map.empty lambdaTerm newLambdaTerm
        then newLambdaTerm
        else f newLambdaTerm
    where newLambdaTerm = normalOrderBetaReduce lambdaTerm

main :: IO ()
main = do
    print lambdaTermOfNumber27
    print (getResult lambdaTermOfNumber27)
