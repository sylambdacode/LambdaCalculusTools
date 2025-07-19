module LambdaTerm where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

data LambdaTerm = Variable String | Abstraction String LambdaTerm | Application LambdaTerm LambdaTerm

fvSet :: Set String -> LambdaTerm -> Set String
fvSet bvSet (Variable name) =
    if name `Set.member` bvSet
        then Set.empty
        else Set.singleton name

fvSet bvSet (Abstraction variableName lambdaTerm) = fvSet newBvSet lambdaTerm
    where newBvSet = Set.insert variableName bvSet

fvSet bvSet (Application functionLambdaTerm argumentLambdaTerm) =
    Set.union (fvSet' functionLambdaTerm) (fvSet' argumentLambdaTerm)
        where fvSet' = fvSet bvSet


substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm

substitute (Variable name) variableName lambdaTerm =
    if name == variableName
        then lambdaTerm
        else Variable name

substitute (Application functionLambdaTerm argumentLambdaTerm) variableName lambdaTerm =
    Application (substitute functionLambdaTerm variableName lambdaTerm) (substitute argumentLambdaTerm variableName lambdaTerm)

substitute (Abstraction variableName bodyLambdaTerm) substitutedVariableName lambdaTerm
    | variableName == substitutedVariableName = Abstraction variableName bodyLambdaTerm
    | not (substitutedVariableName `Set.member` bodyLambdaTermFvSet) = Abstraction variableName bodyLambdaTerm
    | not (variableName `Set.member` lambdaTermFvSet) = Abstraction variableName (substitute bodyLambdaTerm substitutedVariableName lambdaTerm)
    -- | variableName `Set.member` lambdaTermFvSet = Abstraction newVariableName (substitute newBodyLambdaTerm substitutedVariableName lambdaTerm)
    | otherwise = Abstraction newVariableName (substitute newBodyLambdaTerm substitutedVariableName lambdaTerm)
        where newBodyLambdaTerm = substitute bodyLambdaTerm variableName (Variable newVariableName)
              bodyLambdaTermFvSet = fvSet Set.empty bodyLambdaTerm
              lambdaTermFvSet = fvSet Set.empty lambdaTerm
              newVariableName = getNewVariableName variableName
              getNewVariableName name = if not (name `Set.member` lambdaTermFvSet) then name else getNewVariableName (name ++ "_")



alphaEq :: Map String String -> LambdaTerm -> LambdaTerm -> Bool

alphaEq variableNameMap (Variable name1) (Variable name2) =
    case Map.lookup name1 variableNameMap of
        Just name1' -> name1' == name2
        Nothing -> name1 == name2

alphaEq variableNameMap (Abstraction variableName1 lambdaTerm1) (Abstraction variableName2 lambdaTerm2) =
    alphaEq newVariableNameMap lambdaTerm1 lambdaTerm2
        where newVariableNameMap = Map.insert variableName1 variableName2 variableNameMap

alphaEq variableNameMap (Application functionLambdaTerm1 argumentLambdaTerm1) (Application functionLambdaTerm2 argumentLambdaTerm2) =
    alphaEq' functionLambdaTerm1 functionLambdaTerm2 && alphaEq' argumentLambdaTerm1 argumentLambdaTerm2
        where alphaEq' = alphaEq variableNameMap

alphaEq _ _ _ = False


instance Show LambdaTerm where
    show (Variable name) = name
    show (Abstraction variableName lambdaTerm) = "(λ" ++ variableName ++ "." ++ show lambdaTerm ++ ")"
    show (Application functionLambdaTerm argumentLambdaTerm) =
        "(" ++ show functionLambdaTerm ++ " " ++ show argumentLambdaTerm ++ ")"