module LambdaCubeType where

import LambdaCubeTerm
import LambdaCubeReduction

import Data.Map(Map)
import qualified Data.Map as Map



checkType :: [(LambdaCubeTerm, LambdaCubeTerm)] -> Map String LambdaCubeTerm -> LambdaCubeTerm -> Maybe LambdaCubeTerm

checkType _ variableTypeMap (Variable name) =
    case Map.lookup name variableTypeMap of
        Just variableType -> Just variableType
        Nothing -> Nothing

checkType ruleList variableTypeMap (Abstraction variableName variableType lambdaTerm) =
    case bodyType of
        Just bodyType' ->
            let result = DependentProduct variableName variableType bodyType' in
                case checkType ruleList newVariableTypeMap result of
                    Just _ -> Just result
                    Nothing -> Nothing
        Nothing -> Nothing
    where newVariableTypeMap = Map.insert variableName variableType variableTypeMap
          bodyType = checkType ruleList newVariableTypeMap lambdaTerm


checkType ruleList variableTypeMap (DependentProduct variableName variableType lambdaTerm) =
    case bodyType of
        Just bodyType' ->
            case variableTypeSort of
                Just variableTypeSort' ->
                    if checkRule (variableTypeSort', bodyType') ruleList then bodyType else Nothing
                Nothing -> Nothing
        Nothing -> Nothing
    where newVariableTypeMap = Map.insert variableName variableType variableTypeMap
          bodyType = checkType ruleList newVariableTypeMap lambdaTerm
          variableTypeSort = checkType ruleList newVariableTypeMap variableType
          checkRule = \(t1, t2) ruleList' ->
              case ruleList' of
                  [] -> False
                  ((t1', t2') : xs) -> if alphaEq Map.empty t1 t1' && alphaEq Map.empty t2 t2' then True else checkRule (t1, t2) xs


checkType ruleList variableTypeMap (Application functionLambdaTerm argumentLambdaTerm) =
    case functionLambdaTermType of
        Just (DependentProduct _ variableType _) ->
            case (alphaEq Map.empty variableTypeNormalTerm) <$> argumentLambdaTermTypeNormalTerm of
                Just True -> normalOrderBetaReduce <$> (Application <$> functionLambdaTermType <*> Just argumentLambdaTerm)
                _ -> Nothing
            where variableTypeNormalTerm = calculateNormalResult variableType
                  argumentLambdaTermTypeNormalTerm = calculateNormalResult <$> argumentLambdaTermType
        _ -> Nothing
    where functionLambdaTermType = calculateNormalResult <$> checkType ruleList variableTypeMap functionLambdaTerm
          argumentLambdaTermType = calculateNormalResult <$> checkType ruleList variableTypeMap argumentLambdaTerm



