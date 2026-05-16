module LambdaCubeReduction where

import LambdaCubeTerm

import qualified Data.Map as Map


normalOrderBetaReduce :: LambdaCubeTerm -> LambdaCubeTerm

normalOrderBetaReduce (Variable name) = Variable name

normalOrderBetaReduce (Abstraction variableName variableType lambdaTerm) =
    if alphaEq Map.empty lambdaTerm lambdaTermResult
        then Abstraction variableName (normalOrderBetaReduce variableType) lambdaTerm
        else Abstraction variableName variableType lambdaTermResult
    where lambdaTermResult = normalOrderBetaReduce lambdaTerm

normalOrderBetaReduce (DependentProduct variableName variableType lambdaTerm) =
    if alphaEq Map.empty lambdaTerm lambdaTermResult
        then DependentProduct variableName (normalOrderBetaReduce variableType) lambdaTerm
        else DependentProduct variableName variableType lambdaTermResult
    where lambdaTermResult = normalOrderBetaReduce lambdaTerm

normalOrderBetaReduce (Application (Abstraction variableName _ bodyLambdaTerm) argumentLambdaTerm) =
    substitute bodyLambdaTerm variableName argumentLambdaTerm

normalOrderBetaReduce (Application (DependentProduct variableName _ bodyLambdaTerm) argumentLambdaTerm) =
    substitute bodyLambdaTerm variableName argumentLambdaTerm

normalOrderBetaReduce (Application functionLambdaTerm argumentLambdaTerm) =
    if alphaEq Map.empty functionLambdaTerm functionLambdaTermResult
        then Application functionLambdaTerm (normalOrderBetaReduce argumentLambdaTerm)
        else Application functionLambdaTermResult argumentLambdaTerm
    where functionLambdaTermResult = normalOrderBetaReduce functionLambdaTerm



calculateNormalResult :: LambdaCubeTerm -> LambdaCubeTerm

calculateNormalResult lambdaTerm =
    if alphaEq Map.empty lambdaTerm newLambdaTerm
        then newLambdaTerm
        else calculateNormalResult newLambdaTerm
    where newLambdaTerm = normalOrderBetaReduce lambdaTerm
