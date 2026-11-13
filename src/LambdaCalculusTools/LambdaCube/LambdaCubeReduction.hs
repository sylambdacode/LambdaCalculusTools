{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaCalculusTools.LambdaCube.LambdaCubeReduction where

import LambdaCalculusTools.LambdaCube.LambdaCubeTerm


normalOrderBetaReduce :: LambdaCubeTerm -> LambdaCubeTerm

normalOrderBetaReduce (Variable name) = Variable name

normalOrderBetaReduce (Abstraction variableName variableType lambdaTerm) =
    if isNormalForm lambdaTerm
        then Abstraction variableName (normalOrderBetaReduce variableType) lambdaTerm
        else Abstraction variableName variableType lambdaTermResult
    where lambdaTermResult = normalOrderBetaReduce lambdaTerm

normalOrderBetaReduce (DependentProduct variableName variableType lambdaTerm) =
    if isNormalForm lambdaTerm
        then DependentProduct variableName (normalOrderBetaReduce variableType) lambdaTerm
        else DependentProduct variableName variableType lambdaTermResult
    where lambdaTermResult = normalOrderBetaReduce lambdaTerm

normalOrderBetaReduce (Application (Abstraction variableName _ bodyLambdaTerm) argumentLambdaTerm) =
    substitute bodyLambdaTerm variableName argumentLambdaTerm

normalOrderBetaReduce (Application (DependentProduct variableName _ bodyLambdaTerm) argumentLambdaTerm) =
    substitute bodyLambdaTerm variableName argumentLambdaTerm

normalOrderBetaReduce (Application functionLambdaTerm argumentLambdaTerm) =
    if isNormalForm functionLambdaTerm
        then Application functionLambdaTerm (normalOrderBetaReduce argumentLambdaTerm)
        else Application functionLambdaTermResult argumentLambdaTerm
    where functionLambdaTermResult = normalOrderBetaReduce functionLambdaTerm


isNormalForm :: LambdaCubeTerm -> Bool
isNormalForm (Variable _) = True
isNormalForm (Abstraction _ variableType bodyLambdaTerm) = isNormalForm variableType && isNormalForm bodyLambdaTerm
isNormalForm (DependentProduct _ variableType bodyLambdaTerm) = isNormalForm variableType && isNormalForm bodyLambdaTerm
isNormalForm (Application (Abstraction _ _ _) _) = False
isNormalForm (Application (DependentProduct _ _ _) _) = False
isNormalForm (Application functionLambdaTerm argumentLambdaTerm) = isNormalForm functionLambdaTerm && isNormalForm argumentLambdaTerm


calculateNormalResult :: LambdaCubeTerm -> LambdaCubeTerm

calculateNormalResult lambdaTerm =
    if isNormalForm lambdaTerm
        then lambdaTerm
        else calculateNormalResult newLambdaTerm
    where newLambdaTerm = normalOrderBetaReduce lambdaTerm
