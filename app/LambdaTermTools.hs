module LambdaTermTools where

import LambdaTerm(LambdaTerm)
import qualified LambdaTerm as LambdaTerm
import DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified DeBruijnLambdaTerm as DeBruijnLambdaTerm

import Data.Map (Map)
import qualified Data.Map as Map


lambdaTermToDeBruijnLambdaTerm :: Map String Int -> [Map String Int] -> LambdaTerm -> DeBruijnLambdaTerm
lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack (LambdaTerm.Variable name) =
    case Map.lookup name globalFreeVariableMap of
        Nothing ->
            case Map.lookup name (head variableMapStack) of
                Just variableLevel -> DeBruijnLambdaTerm.Variable (length variableMapStack - variableLevel)
                Nothing -> DeBruijnLambdaTerm.Variable 0
        Just index -> DeBruijnLambdaTerm.Variable index
lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack (LambdaTerm.Abstraction name lambdaTerm) =
    let variableMap = Map.insert name (length variableMapStack) (head variableMapStack)
    in DeBruijnLambdaTerm.Abstraction (lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap (variableMap : variableMapStack) lambdaTerm)
lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack (LambdaTerm.Application functionLambdaTerm argumentLambdaTerm) =
    DeBruijnLambdaTerm.Application (lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack functionLambdaTerm) (lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack argumentLambdaTerm)


