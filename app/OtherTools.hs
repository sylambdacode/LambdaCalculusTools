module OtherTools where

import LambdaTerm
import LambdaReduction

import qualified Data.Map as Map

nilLambdaTerm :: LambdaTerm
nilLambdaTerm = Abstraction "n" (Abstraction "c" (Variable "c"))

isNil :: LambdaTerm -> Bool
isNil lambdaTerm = alphaEq Map.empty lambdaTerm nilLambdaTerm

headLambdaTerm :: LambdaTerm
headLambdaTerm = Abstraction "x" (Abstraction "xs" (Variable "x"))
listHead :: LambdaTerm -> LambdaTerm
listHead lambdaTerm = Application lambdaTerm headLambdaTerm

tailLambdaTerm :: LambdaTerm
tailLambdaTerm = Abstraction "x" (Abstraction "xs" (Variable "xs"))
listTail :: LambdaTerm -> LambdaTerm
listTail lambdaTerm = Application lambdaTerm tailLambdaTerm


toInt' :: LambdaTerm -> Int
toInt' (Variable _) = 0
toInt' (Application (Variable _) body) = 1 + (toInt' body)
toInt' _ = error "Not Number"

toInt :: LambdaTerm -> Int
toInt (Abstraction _ (Abstraction _ body)) = toInt' body
toInt _ = error "Not Number"

toBool :: LambdaTerm -> Bool
toBool (Abstraction x (Abstraction y (Variable c))) = x == c
toBool _ = error "Not Bool"

toBoolList :: LambdaTerm -> [Bool]
toBoolList lambdaTerm =
    if isNil lambdaTerm
    then []
    else (toBool $ calculateNormalResult $ listHead $ lambdaTerm) : (toBoolList (calculateNormalResult $ listTail lambdaTerm))
