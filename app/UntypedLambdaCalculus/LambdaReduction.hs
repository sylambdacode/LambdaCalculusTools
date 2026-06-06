module UntypedLambdaCalculus.LambdaReduction where

import UntypedLambdaCalculus.LambdaTerm

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

isWeakNormalHeadForm :: LambdaTerm -> Bool
isWeakNormalHeadForm (Variable _) = True
isWeakNormalHeadForm (Abstraction _ _) = True
isWeakNormalHeadForm (Application (Abstraction _ _) _) = False
isWeakNormalHeadForm (Application functionLambdaTerm _) = isWeakNormalHeadForm functionLambdaTerm

calculateWeakNormalHeadResult :: LambdaTerm -> LambdaTerm
calculateWeakNormalHeadResult lambdaTerm =
    if isWeakNormalHeadForm lambdaTerm
        then lambdaTerm
        else calculateWeakNormalHeadResult newLambdaTerm
    where newLambdaTerm = normalOrderBetaReduce lambdaTerm

isNormalForm :: LambdaTerm -> Bool
isNormalForm (Variable _) = True
isNormalForm (Abstraction _ bodyLambdaTerm) = isNormalForm bodyLambdaTerm
isNormalForm (Application (Abstraction _ _) _) = False
isNormalForm (Application functionLambdaTerm argumentLambdaTerm) = isNormalForm functionLambdaTerm && isNormalForm argumentLambdaTerm

calculateNormalResult :: LambdaTerm -> LambdaTerm
calculateNormalResult lambdaTerm =
    if isNormalForm lambdaTerm
        then lambdaTerm
        else calculateNormalResult newLambdaTerm
    where newLambdaTerm = normalOrderBetaReduce lambdaTerm
