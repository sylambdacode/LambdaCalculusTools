module SimpleTypedLambdaTerm where

import SimpleType

import Data.Map (Map)
import qualified Data.Map as Map


data SimpleTypedLambdaTerm = Variable String | Abstraction String SimpleTypedLambdaTerm SimpleType | Application SimpleTypedLambdaTerm SimpleTypedLambdaTerm


checkSimpleType :: Map String SimpleType -> SimpleTypedLambdaTerm -> Maybe SimpleType

checkSimpleType variableTypeMap (Variable name) =
    case Map.lookup name variableTypeMap of
        Just simpleType -> Just simpleType
        Nothing -> Nothing

checkSimpleType variableTypeMap (Abstraction variableName lambdaTerm simpleType) =
    case bodySimpleType of
        Just bodySimpleType' -> Just (FunctionType simpleType bodySimpleType')
        Nothing -> Nothing
    where newVariableTypeMap = Map.insert variableName simpleType variableTypeMap
          bodySimpleType = checkSimpleType newVariableTypeMap lambdaTerm

checkSimpleType variableTypeMap (Application functionLambdaTerm argumentLambdaTerm) = do
    case functionLambdaTermSimpleType of
        Just (FunctionType argumentLambdaTermSimpleType' simpleType) -> do
            case (simpleTypeEq argumentLambdaTermSimpleType') <$> argumentLambdaTermSimpleType of
                Just True -> Just simpleType
                _ -> Nothing
        _ -> Nothing
    where functionLambdaTermSimpleType = checkSimpleType variableTypeMap functionLambdaTerm
          argumentLambdaTermSimpleType = checkSimpleType variableTypeMap argumentLambdaTerm


instance Show SimpleTypedLambdaTerm where
    show (Variable name) = name
    show (Abstraction variableName lambdaTerm simpleType) = "(λ" ++ variableName ++ ":" ++ show simpleType ++ "." ++ show lambdaTerm ++ ")"
    show (Application functionLambdaTerm argumentLambdaTerm) = "(" ++ show functionLambdaTerm ++ " " ++ show argumentLambdaTerm ++ ")"
