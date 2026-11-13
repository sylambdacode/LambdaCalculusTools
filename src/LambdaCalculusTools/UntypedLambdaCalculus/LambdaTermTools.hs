{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaCalculusTools.UntypedLambdaCalculus.LambdaTermTools where

import LambdaCalculusTools.UntypedLambdaCalculus.LambdaTerm(LambdaTerm)
import qualified LambdaCalculusTools.UntypedLambdaCalculus.LambdaTerm as LambdaTerm
import LambdaCalculusTools.UntypedLambdaCalculus.DeBruijnLambdaTerm(DeBruijnLambdaTerm)
import qualified LambdaCalculusTools.UntypedLambdaCalculus.DeBruijnLambdaTerm as DeBruijnLambdaTerm

import Data.Map (Map)
import qualified Data.Map as Map


lambdaTermToDeBruijnLambdaTerm :: Map String Int -> [Map String Int] -> LambdaTerm -> DeBruijnLambdaTerm
lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack (LambdaTerm.Variable name) =
    case Map.lookup name globalFreeVariableMap of
        Nothing ->
            case Map.lookup name variableMapStackTop of
                Just variableLevel -> DeBruijnLambdaTerm.Variable (length variableMapStack - variableLevel)
                Nothing -> DeBruijnLambdaTerm.Variable 0
        Just index -> DeBruijnLambdaTerm.Variable index
    where variableMapStackTop = case variableMapStack of
              [] -> error "wrong variable map stack"
              x : _ -> x

lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack (LambdaTerm.Abstraction name lambdaTerm) =
    let variableMap = Map.insert name (length variableMapStack) variableMapStackTop
    in DeBruijnLambdaTerm.Abstraction (lambdaTermToDeBruijnLambdaTerm' (variableMap : variableMapStack) lambdaTerm)
    where lambdaTermToDeBruijnLambdaTerm' = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap
          variableMapStackTop = case variableMapStack of
              [] -> error "wrong variable map stack"
              x : _ -> x

lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack (LambdaTerm.Application functionLambdaTerm argumentLambdaTerm) =
    DeBruijnLambdaTerm.Application (lambdaTermToDeBruijnLambdaTerm' functionLambdaTerm) (lambdaTermToDeBruijnLambdaTerm' argumentLambdaTerm)
    where lambdaTermToDeBruijnLambdaTerm' = lambdaTermToDeBruijnLambdaTerm globalFreeVariableMap variableMapStack


