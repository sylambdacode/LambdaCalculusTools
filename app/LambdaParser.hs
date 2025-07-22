module LambdaParser where

import LambdaTerm

import Text.ParserCombinators.Parsec
import Data.Char (isLetter, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map

data Expr = Var String | ExprList [Expr] | Lam [String] Expr

instance Show Expr where
    show (Var name) = name
    show (ExprList exprs) = "(" ++ show' exprs ++ ")"
        where show' [] = ""
              show' [e] = show e
              show' (e : es) = show e ++ " " ++ show' es
    show (Lam names expr) = "\\" ++ show names ++ "." ++ show expr

data ValDef = ValDef String Expr

instance Show ValDef where
    show (ValDef name expr) = show name ++ " = " ++ show expr


skipWhiteChar :: Parser String
skipWhiteChar = many $ satisfy (`elem` " \n\t")

parseName :: Parser String
parseName = skipWhiteChar *> many1 satisfy'
    where satisfy' = satisfy (\c -> isLetter c || isDigit c || c `elem` "_-")

parseVar :: Parser Expr
parseVar = Var <$> parseName

parseBracket :: Parser Expr
parseBracket = skipWhiteChar *> char '(' *> parseExprs <* skipWhiteChar <* char ')'

parseExpr :: Parser Expr
parseExpr = try parseVar <|> try parseBracket <|> try parseLam

parseExprs :: Parser Expr
parseExprs = simply . ExprList <$> many1 parseExpr
    where simply (ExprList [expr]) = expr
          simply (ExprList exprs) = ExprList exprs
          simply expr = expr

parseLam :: Parser Expr
parseLam = Lam
    <$> (skipWhiteChar *> char '\\' *> many1 (try parseName))
    <*> (skipWhiteChar *> char '.' *> parseExprs)

parseValDef :: Parser ValDef
parseValDef = ValDef
    <$> parseName
    <*> (skipWhiteChar *> char '=' *> parseExprs <* skipWhiteChar <* char ';')

parseValDefs :: Parser [ValDef]
parseValDefs = many parseValDef

parseCode :: SourceName -> String -> Either ParseError [ValDef]
parseCode codeName code = parse parseValDefs codeName code

lambdaTermList :: [LambdaTerm] -> LambdaTerm
lambdaTermList [] = error "wrong input(lambdaTermList)"
lambdaTermList (lambdaTerm : otherLambdaTermList) =
    foldl (\l r -> Application l r) lambdaTerm otherLambdaTermList


lambdaFunction :: [String] -> LambdaTerm -> LambdaTerm
lambdaFunction [] _ = error "wrong input(lambdaFunction)"
lambdaFunction (variableName : []) bodyLambdaTerm =
    Abstraction variableName bodyLambdaTerm
lambdaFunction (variableName : otherVariableNameList) bodyLambdaTerm =
    Abstraction variableName (lambdaFunction otherVariableNameList bodyLambdaTerm)



toLambdaTerm :: Map String Expr -> Expr -> LambdaTerm
toLambdaTerm valDefMap (Var name) =
    case Map.lookup name valDefMap of
        Just expr -> toLambdaTerm valDefMap expr
        Nothing -> Variable name
toLambdaTerm valDefMap (ExprList exprs) = lambdaTermList (map (toLambdaTerm valDefMap) exprs)
toLambdaTerm valDefMap (Lam names expr) = lambdaFunction names (toLambdaTerm valDefMap expr)


valDefListToMap :: [ValDef] -> Map String Expr
valDefListToMap valDefList = Map.fromList (map valDefToPair valDefList)
    where valDefToPair (ValDef name expr) = (name, expr)
