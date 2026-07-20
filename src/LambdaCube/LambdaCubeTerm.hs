module LambdaCube.LambdaCubeTerm where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


data LambdaCubeTerm = Variable String
                    | Abstraction String LambdaCubeTerm LambdaCubeTerm
                    | DependentProduct String LambdaCubeTerm LambdaCubeTerm
                    | Application LambdaCubeTerm LambdaCubeTerm

fvSet :: Set String -> LambdaCubeTerm -> Set String
fvSet bvSet (Variable name) =
    if name `Set.member` bvSet
        then Set.empty
        else Set.singleton name

fvSet bvSet (Abstraction variableName variableType lambdaTerm) = Set.union (fvSet newBvSet lambdaTerm) (fvSet newBvSet variableType)
    where newBvSet = Set.insert variableName bvSet

fvSet bvSet (DependentProduct variableName variableType lambdaTerm) = Set.union (fvSet newBvSet lambdaTerm) (fvSet newBvSet variableType)
    where newBvSet = Set.insert variableName bvSet

fvSet bvSet (Application functionLambdaTerm argumentLambdaTerm) =
    Set.union (fvSet' functionLambdaTerm) (fvSet' argumentLambdaTerm)
        where fvSet' = fvSet bvSet


substitute :: LambdaCubeTerm -> String -> LambdaCubeTerm -> LambdaCubeTerm

substitute (Variable name) variableName lambdaTerm =
    if name == variableName
        then lambdaTerm
        else Variable name

substitute (Application functionLambdaTerm argumentLambdaTerm) variableName lambdaTerm =
    Application (substitute functionLambdaTerm variableName lambdaTerm) (substitute argumentLambdaTerm variableName lambdaTerm)

substitute (Abstraction variableName variableType bodyLambdaTerm) substitutedVariableName lambdaTerm
    | variableName == substitutedVariableName = Abstraction variableName variableType bodyLambdaTerm
    | not (substitutedVariableName `Set.member` bodyAndTypeLambdaTermFvSet) = Abstraction variableName variableType bodyLambdaTerm
    | not (variableName `Set.member` lambdaTermFvSet) = Abstraction variableName (substitute variableType substitutedVariableName lambdaTerm) (substitute bodyLambdaTerm substitutedVariableName lambdaTerm)
    | otherwise = Abstraction newVariableName (substitute newVariableType substitutedVariableName lambdaTerm) (substitute newBodyLambdaTerm substitutedVariableName lambdaTerm)
        where newBodyLambdaTerm = substitute bodyLambdaTerm variableName (Variable newVariableName)
              newVariableType = substitute variableType variableName (Variable newVariableName)
              bodyAndTypeLambdaTermFvSet = Set.union (fvSet Set.empty bodyLambdaTerm) (fvSet Set.empty variableType)
              lambdaTermFvSet = fvSet Set.empty lambdaTerm
              newVariableName = getNewVariableName variableName
              getNewVariableName name = if not (name `Set.member` lambdaTermFvSet) then name else getNewVariableName (name ++ "_")

substitute (DependentProduct variableName variableType bodyLambdaTerm) substitutedVariableName lambdaTerm
    | variableName == substitutedVariableName = DependentProduct variableName variableType bodyLambdaTerm
    | not (substitutedVariableName `Set.member` bodyAndTypeLambdaTermFvSet) = DependentProduct variableName variableType bodyLambdaTerm
    | not (variableName `Set.member` lambdaTermFvSet) = DependentProduct variableName (substitute variableType substitutedVariableName lambdaTerm) (substitute bodyLambdaTerm substitutedVariableName lambdaTerm)
    | otherwise = DependentProduct newVariableName (substitute newVariableType substitutedVariableName lambdaTerm) (substitute newBodyLambdaTerm substitutedVariableName lambdaTerm)
        where newBodyLambdaTerm = substitute bodyLambdaTerm variableName (Variable newVariableName)
              newVariableType = substitute variableType variableName (Variable newVariableName)
              bodyAndTypeLambdaTermFvSet = Set.union (fvSet Set.empty bodyLambdaTerm) (fvSet Set.empty variableType)
              lambdaTermFvSet = fvSet Set.empty lambdaTerm
              newVariableName = getNewVariableName variableName
              getNewVariableName name = if not (name `Set.member` lambdaTermFvSet) then name else getNewVariableName (name ++ "_")



alphaEq :: Map String String -> LambdaCubeTerm -> LambdaCubeTerm -> Bool

alphaEq variableNameMap (Variable name1) (Variable name2) =
    case Map.lookup name1 variableNameMap of
        Just name1' -> name1' == name2
        Nothing -> name1 == name2

alphaEq variableNameMap (Abstraction variableName1 variableType1 lambdaTerm1) (Abstraction variableName2 variableType2 lambdaTerm2) =
    alphaEq newVariableNameMap lambdaTerm1 lambdaTerm2 && alphaEq newVariableNameMap variableType1 variableType2
        where newVariableNameMap = Map.insert variableName1 variableName2 variableNameMap

alphaEq variableNameMap (DependentProduct variableName1 variableType1 lambdaTerm1) (DependentProduct variableName2 variableType2 lambdaTerm2) =
    alphaEq newVariableNameMap lambdaTerm1 lambdaTerm2 && alphaEq newVariableNameMap variableType1 variableType2
        where newVariableNameMap = Map.insert variableName1 variableName2 variableNameMap

alphaEq variableNameMap (Application functionLambdaTerm1 argumentLambdaTerm1) (Application functionLambdaTerm2 argumentLambdaTerm2) =
    alphaEq' functionLambdaTerm1 functionLambdaTerm2 && alphaEq' argumentLambdaTerm1 argumentLambdaTerm2
        where alphaEq' = alphaEq variableNameMap

alphaEq _ _ _ = False


instance Show LambdaCubeTerm where
    show (Variable name) = name
    show (Abstraction variableName variableType lambdaTerm) = "(λ" ++ variableName ++ ":" ++ show variableType ++ "." ++ show lambdaTerm ++ ")"
    show (DependentProduct variableName variableType lambdaTerm) = "(Π" ++ variableName ++ ":" ++ show variableType ++ "." ++ show lambdaTerm ++ ")"
    show (Application functionLambdaTerm argumentLambdaTerm) = "(" ++ show functionLambdaTerm ++ " " ++ show argumentLambdaTerm ++ ")"
