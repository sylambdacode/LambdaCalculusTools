module UntypedLambdaCalculus.LambdaTerm where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

data LambdaTerm = Variable String | Abstraction String LambdaTerm | Application LambdaTerm LambdaTerm

fvSet :: LambdaTerm -> Set String
fvSet (Variable name) = Set.singleton name

fvSet (Abstraction variableName lambdaTerm) = Set.delete variableName (fvSet lambdaTerm)

fvSet (Application functionLambdaTerm argumentLambdaTerm) =
    Set.union (fvSet functionLambdaTerm) (fvSet argumentLambdaTerm)


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
              bodyLambdaTermFvSet = fvSet bodyLambdaTerm
              lambdaTermFvSet = fvSet lambdaTerm
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
    show lambdaTerm = showLambdaTerm lambdaTerm

showVariableName :: String -> String
showVariableName [] = []
showVariableName (c : xs) =
    if c == 'λ' || c == ' ' || c == '(' || c == ')' || c == '.' || c == '\\'
        then '\\' : c : (showVariableName xs)
        else c : (showVariableName xs)

showLambdaTerm :: LambdaTerm -> String
showLambdaTerm (Variable name) = showVariableName name
showLambdaTerm (Abstraction variableName lambdaTerm) =
    "(λ" ++ showVariableName variableName ++ "." ++ show lambdaTerm ++ ")"
showLambdaTerm (Application functionLambdaTerm argumentLambdaTerm) =
    "(" ++ show functionLambdaTerm ++ " " ++ show argumentLambdaTerm ++ ")"


readLambdaTerm :: String -> LambdaTerm
readLambdaTerm lambdaTermString =
    case parseLambdaTerm lambdaTermString of
        (Just lambdaTerm, _) -> lambdaTerm
        (Nothing, _) -> error "parse LambdaTerm String error"

parseVariableName :: String -> Bool -> (Maybe String, String)
parseVariableName [] _ = (Nothing, [])
parseVariableName (c : xs) True =
    if c == 'λ' || c == ' ' || c == '(' || c == ')' || c == '.' || c == '\\'
        then case parseVariableName xs False of
                 (Just result, reminder) -> (Just (c : result), reminder)
                 (Nothing, reminder) -> (Just [c], reminder)
        else (Nothing, (c : xs))
parseVariableName (c : xs) False =
    if c == 'λ' || c == ' ' || c == '(' || c == ')' || c == '.'
        then (Nothing, (c : xs))
        else if c == '\\'
                 then case parseVariableName xs True of
                          (Just result, reminder) -> (Just result, reminder)
                          (Nothing, _) -> (Nothing, (c : xs))
                 else case parseVariableName xs False of
                          (Just result, reminder) -> (Just (c : result), reminder)
                          (Nothing, reminder) -> (Just [c], reminder)

parseVariable :: String -> (Maybe LambdaTerm, String)
parseVariable lambdaTermString =
    case parseVariableName lambdaTermString False of
        (Just result, reminder) -> (Just (Variable result), reminder)
        (Nothing, reminder) -> (Nothing, reminder)

parseAbstraction :: String -> (Maybe LambdaTerm, String)
parseAbstraction lambdaTermString =
    case lambdaTermString of
        ('(' : 'λ' : reminder1) ->
            case parseVariableName reminder1 False of
                (Just name, '.' : reminder2) ->
                    case parseLambdaTerm reminder2 of
                        (Just bodyLambdaTerm, ')' : reminder3) -> (Just (Abstraction name bodyLambdaTerm), reminder3)
                        (Just _, reminder3) -> (Nothing, reminder3)
                        (Nothing, reminder3) -> (Nothing, reminder3)
                (Just _, reminder2) -> (Nothing, reminder2)
                (Nothing, reminder2) -> (Nothing, reminder2)
        _ -> (Nothing, lambdaTermString)

parseApplication :: String -> (Maybe LambdaTerm, String)
parseApplication lambdaTermString =
    case lambdaTermString of
        ('(' : reminder1) ->
            case parseLambdaTerm reminder1 of
                (Just functionLambdaTerm, ' ' : reminder2) ->
                    case parseLambdaTerm reminder2 of
                        (Just argumentLambdaTerm, ')' : reminder3) -> (Just (Application functionLambdaTerm argumentLambdaTerm), reminder3)
                        (Just _, reminder3) -> (Nothing, reminder3)
                        (Nothing, reminder3) -> (Nothing, reminder3)
                (Just _, reminder2) -> (Nothing, reminder2)
                (Nothing, reminder2) -> (Nothing, reminder2)
        _ -> (Nothing, lambdaTermString)


parseLambdaTerm :: String -> (Maybe LambdaTerm, String)
parseLambdaTerm lambdaTermString =
    case parseVariable lambdaTermString of
        (Just result, reminder) -> (Just result, reminder)
        (Nothing, _) ->
            case parseAbstraction lambdaTermString of
                (Just result, reminder) -> (Just result, reminder)
                (Nothing, _) ->
                    case parseApplication lambdaTermString of
                        (Just result, reminder) -> (Just result, reminder)
                        (Nothing, reminder) -> (Nothing, reminder)


