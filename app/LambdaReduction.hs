module LambdaReduction where

import LambdaTerm

import qualified Data.Map as Map


normalOrderBetaReduce :: LambdaTerm -> LambdaTerm
normalOrderBetaReduce (Variable name) = Variable name
normalOrderBetaReduce (Abstraction variableName lambdaTerm) = Abstraction variableName (normalOrderBetaReduce lambdaTerm)
normalOrderBetaReduce (Application (Abstraction variableName bodyLambdaTerm) argumentLambdaTerm) =
    substitute bodyLambdaTerm variableName argumentLambdaTerm
normalOrderBetaReduce (Application functionLambdaTerm argumentLambdaTerm) =
    if alphaEq Map.empty functionLambdaTerm functionLambdaTermResult
        then Application functionLambdaTerm (normalOrderBetaReduce argumentLambdaTerm)
        else Application functionLambdaTermResult argumentLambdaTerm
    where functionLambdaTermResult = normalOrderBetaReduce functionLambdaTerm


calculateNormalResult :: LambdaTerm -> LambdaTerm
calculateNormalResult lambdaTerm =
    if alphaEq Map.empty lambdaTerm newLambdaTerm
        then newLambdaTerm
        else calculateNormalResult newLambdaTerm
    where newLambdaTerm = normalOrderBetaReduce lambdaTerm
