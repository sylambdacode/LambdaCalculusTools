{-
本文件内容为开发过程中使用的临时代码
-}


module Main where

import LambdaCubeTerm
import LambdaCubeReduction
import LambdaCubeType

import Data.Map(Map)
import qualified Data.Map as Map

lambdaTerm1 :: LambdaCubeTerm
lambdaTerm1 = Abstraction "X" (Variable "*") (Abstraction "x" (Variable "X") (Variable "x"))

lambdaTerm2 :: LambdaCubeTerm
lambdaTerm2 = Abstraction "f" (DependentProduct "X" (Variable "*") (DependentProduct "x" (Variable "X") (Variable "X"))) (Variable "f")

lambdaTerm3 :: LambdaCubeTerm
lambdaTerm3 = Application lambdaTerm2 lambdaTerm1

lambdaTerm4 :: LambdaCubeTerm
lambdaTerm4 = Application lambdaTerm3 (Variable "Nat")

lambdaTerm5 :: LambdaCubeTerm
lambdaTerm5 = Application lambdaTerm4 (Variable "1")

lambdaResult :: LambdaCubeTerm
lambdaResult = calculateNormalResult lambdaTerm5

typeMap :: Map String LambdaCubeTerm
typeMap = Map.fromList [("*", Variable "□"), ("Nat", Variable "*"), ("1", Variable "Nat")]

ruleList :: [(LambdaCubeTerm, LambdaCubeTerm)]
ruleList = [(Variable "*", Variable "*"),
            (Variable "□", Variable "*")]
            {-(Variable "*", Variable "□"),
            (Variable "□", Variable "□")]-}

lambdaType1 :: Maybe LambdaCubeTerm
lambdaType1 = checkType ruleList typeMap lambdaTerm1

lambdaType2 :: Maybe LambdaCubeTerm
lambdaType2 = checkType ruleList typeMap lambdaTerm5

vecType :: LambdaCubeTerm
vecType = DependentProduct "A" (Variable "*") (DependentProduct "n" (Variable "Nat") (Variable "*"))

vecTypeType :: Maybe LambdaCubeTerm
vecTypeType = checkType ruleList typeMap vecType

main :: IO ()
main = do
    print lambdaTerm5
    print lambdaType2
