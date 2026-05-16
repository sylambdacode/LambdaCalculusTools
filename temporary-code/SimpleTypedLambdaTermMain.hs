{-
本文件内容为开发过程中使用的临时代码
-}

module Main where

import SimpleTypedLambdaCalculus.SimpleType
import SimpleTypedLambdaCalculus.SimpleTypedLambdaTerm
import qualified Data.Map as Map


simpleTypedLambdaTerm1 :: SimpleTypedLambdaTerm
simpleTypedLambdaTerm1 = Abstraction "x" (Variable "x") (AtomicType "B")

simpleTypedLambdaTerm2 :: SimpleTypedLambdaTerm
simpleTypedLambdaTerm2 = Variable "y"

simpleTypedLambdaTerm3 :: SimpleTypedLambdaTerm
simpleTypedLambdaTerm3 = Application simpleTypedLambdaTerm1 simpleTypedLambdaTerm2


main :: IO ()
main = do
    print simpleTypedLambdaTerm3
    print (checkSimpleType (Map.singleton "y" (AtomicType "A")) simpleTypedLambdaTerm3)


