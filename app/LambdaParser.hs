module LambdaParser where

import LambdaTerm

import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map

data Expr = Var String | ExprList [Expr] | Lam [String] Expr | Do [DoItem] Expr | Let ValDef Expr
data DoItem = DoItem [String] Expr

instance Show Expr where
    show (Var name) = name
    show (ExprList exprs) = "(" ++ show' exprs ++ ")"
        where show' [] = ""
              show' [e] = show e
              show' (e : es) = show e ++ " " ++ show' es
    show (Lam names expr) = "\\" ++ show names ++ "." ++ show expr
    show (Do items finalExprs) = "do" ++ " " ++ show items ++ " > " ++ show finalExprs
    show (Let valDef expr) = "let " ++ show valDef ++ " in " ++ show expr

instance Show DoItem where
    show (DoItem names expr) = show names ++ " <- " ++ show expr

data ValDef = ValDef String Expr

instance Show ValDef where
    show (ValDef name expr) = show name ++ " = " ++ show expr


skipComment :: Parser Char
skipComment = char '{' *> char '-' *> skipComment'
    where skipComment' =
            (many $ satisfy (\c -> not (c == '-')))
            *> char '-'
            *> ((try $ char '}') <|> skipComment')

skipWhiteChar :: Parser String
skipWhiteChar = many $ satisfy (`elem` " \n\r\t")

skipWhiteCharAndComment :: Parser ()
skipWhiteCharAndComment = try (skipWhiteChar *> many (skipComment *> skipWhiteChar) >> return ())
    <|> try (skipWhiteChar >> return ())

parseName :: Parser String
parseName = skipWhiteCharAndComment *> many1 (noneOf "λ\\.(){}=;< \n\r\t")

parseVar :: Parser Expr
parseVar = Var <$> parseName

parseBracket :: Parser Expr
parseBracket = skipWhiteCharAndComment *> char '(' *> parseExprs <* skipWhiteCharAndComment <* char ')'

parseExpr :: Parser Expr
parseExpr = try parseDo <|> try parseLet <|> try parseVar <|> try parseBracket <|> try parseLam

parseExprs :: Parser Expr
parseExprs = simply . ExprList <$> many1 parseExpr
    where simply (ExprList [expr]) = expr
          simply (ExprList exprs) = ExprList exprs
          simply expr = expr

parseLam :: Parser Expr
parseLam = Lam
    <$> (skipWhiteCharAndComment *> oneOf "\\λ" *> many1 (try parseName))
    <*> (skipWhiteCharAndComment *> char '.' *> parseExprs)

parseDo :: Parser Expr
parseDo = Do <$> (skipWhiteCharAndComment *> char 'd' *> char 'o' *> skipWhiteCharAndComment *> char '{'
    *> many1 (try parseDoItem)) <*> (parseExprs <* skipWhiteCharAndComment
        <* char '}')

parseDoItem :: Parser DoItem
parseDoItem = DoItem
    <$> many1 (try parseName)
    <*> (skipWhiteCharAndComment *> char '<' *> char '-' *> parseExprs <* skipWhiteCharAndComment <* char ';')

parseLet :: Parser Expr
parseLet = Let
    <$> (skipWhiteCharAndComment *> char 'l' *> char 'e' *> char 't' *> parseValDef)
    <*> (skipWhiteCharAndComment *> char 'i' *> char 'n' *> parseExprs)

parseValDef :: Parser ValDef
parseValDef = ValDef
    <$> parseName
    <*> (skipWhiteCharAndComment *> char '=' *> parseExprs <* skipWhiteCharAndComment <* char ';')

parseValDefs :: Parser [ValDef]
parseValDefs = many $ try parseValDef

parseCode :: SourceName -> String -> Either ParseError [ValDef]
parseCode codeName code = parse (parseValDefs <* skipWhiteCharAndComment <* eof) codeName code

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
toLambdaTerm valDefMap (Do [] finalExprs) = toLambdaTerm valDefMap finalExprs
toLambdaTerm valDefMap (Do ((DoItem names expr) : xs) finalExprs) =
    Application (toLambdaTerm valDefMap expr)
        (lambdaFunction names (toLambdaTerm valDefMap (Do xs finalExprs)))
toLambdaTerm valDefMap (Let (ValDef name val) expr) = Application (Abstraction name (toLambdaTerm valDefMap expr)) (toLambdaTerm valDefMap val)


valDefListToMap :: [ValDef] -> Map String Expr
valDefListToMap valDefList = Map.fromList (map valDefToPair valDefList)
    where valDefToPair (ValDef name expr) = (name, expr)
