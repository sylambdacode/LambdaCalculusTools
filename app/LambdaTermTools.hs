module LambdaTermTools where

import LambdaTerm(LambdaTerm)
import qualified LambdaTerm as LambdaTerm
import DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified DeBruijnLambdaTerm as DeBruijnLambdaTerm

import Data.Map (Map)
import qualified Data.Map as Map


lambdaTermToDeBruijnLambdaTerm :: [Map String Int] -> LambdaTerm -> DeBruijnLambdaTerm
lambdaTermToDeBruijnLambdaTerm variableMapStack (LambdaTerm.Variable name) =
    case Map.lookup name (head variableMapStack) of
        Just variableLevel -> DeBruijnLambdaTerm.Variable (length variableMapStack - variableLevel)
        Nothing -> DeBruijnLambdaTerm.Variable (-1)
lambdaTermToDeBruijnLambdaTerm variableMapStack (LambdaTerm.Abstraction name lambdaTerm) =
    let variableMap = Map.insert name (length variableMapStack) (head variableMapStack)
    in DeBruijnLambdaTerm.Abstraction (lambdaTermToDeBruijnLambdaTerm (variableMap : variableMapStack) lambdaTerm)
lambdaTermToDeBruijnLambdaTerm variableMapStack (LambdaTerm.Application functionLambdaTerm argumentLambdaTerm) =
    DeBruijnLambdaTerm.Application (lambdaTermToDeBruijnLambdaTerm variableMapStack functionLambdaTerm) (lambdaTermToDeBruijnLambdaTerm variableMapStack argumentLambdaTerm)


