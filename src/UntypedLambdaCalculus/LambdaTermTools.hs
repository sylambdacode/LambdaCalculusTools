module UntypedLambdaCalculus.LambdaTermTools where

import UntypedLambdaCalculus.LambdaTerm(LambdaTerm)
import qualified UntypedLambdaCalculus.LambdaTerm as LambdaTerm
import UntypedLambdaCalculus.DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified UntypedLambdaCalculus.DeBruijnLambdaTerm as DeBruijnLambdaTerm

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


