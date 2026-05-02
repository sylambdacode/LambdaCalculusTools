{-
本文件内容为开发过程中使用的临时代码
-}

module Main where

import SimpleType
import SimpleTypedLambdaTerm


simpleTypedLambdaTerm1 :: SimpleTypedLambdaTerm
simpleTypedLambdaTerm1 = Abstraction "x" (Variable "x" (AtomicType "A")) (FunctionType (AtomicType "A") (AtomicType "A"))

simpleTypedLambdaTerm2 :: SimpleTypedLambdaTerm
simpleTypedLambdaTerm2 = Variable "y" (AtomicType "A")

simpleTypedLambdaTerm3 :: SimpleTypedLambdaTerm
simpleTypedLambdaTerm3 = Application simpleTypedLambdaTerm1 simpleTypedLambdaTerm2 (AtomicType "A")


main :: IO ()
main = do
    print simpleTypedLambdaTerm3
    print (checkSimpleType (Map.singleton "y" (AtomicType "A")) simpleTypedLambdaTerm3)
