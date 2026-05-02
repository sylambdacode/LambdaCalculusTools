module SimpleTypedLambdaTerm where

import SimpleType

import Data.Map (Map)
import qualified Data.Map as Map



data SimpleTypedLambdaTerm = Variable String SimpleType | Abstraction String SimpleTypedLambdaTerm SimpleType | Application SimpleTypedLambdaTerm SimpleTypedLambdaTerm SimpleType


simpleTypeTag :: SimpleTypedLambdaTerm -> SimpleType
simpleTypeTag (Variable _ simpleType) = simpleType
simpleTypeTag (Abstraction _ _ simpleType) = simpleType
simpleTypeTag (Application _ _ simpleType) = simpleType


checkSimpleType :: Map String SimpleType -> SimpleTypedLambdaTerm -> Bool

checkSimpleType variableTypeMap (Variable name simpleType) =
    case Map.lookup name variableTypeMap of
        Just simpleType' -> simpleTypeEq simpleType simpleType'
        Nothing -> False

checkSimpleType variableTypeMap (Abstraction variableName lambdaTerm (FunctionType simpleType1 simpleType2)) =
    simpleTypeEq (simpleTypeTag lambdaTerm) simpleType2 &&
    checkSimpleType newVariableTypeMap lambdaTerm
        where newVariableTypeMap = Map.insert variableName simpleType1 variableTypeMap

checkSimpleType variableTypeMap (Application functionLambdaTerm argumentLambdaTerm simpleType) =
    simpleTypeEq functionLambdaTermSimpleType (FunctionType argumentLambdaTermSimpleType simpleType) &&
    checkSimpleType variableTypeMap functionLambdaTerm &&
    checkSimpleType variableTypeMap argumentLambdaTerm
        where functionLambdaTermSimpleType = simpleTypeTag functionLambdaTerm
              argumentLambdaTermSimpleType = simpleTypeTag argumentLambdaTerm

checkSimpleType _ _ = False


instance Show SimpleTypedLambdaTerm where
    show (Variable name simpleType) = name ++ "^" ++ show simpleType
    show (Abstraction variableName lambdaTerm simpleType) = "(λ" ++ variableName ++ "." ++ show lambdaTerm ++ ")" ++ "^" ++ show simpleType
    show (Application functionLambdaTerm argumentLambdaTerm simpleType) =
        "(" ++ show functionLambdaTerm ++ " " ++ show argumentLambdaTerm ++ ")" ++ "^" ++ show simpleType
