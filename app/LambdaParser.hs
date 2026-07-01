module LambdaParser where

import UntypedLambdaCalculus.LambdaTerm

import Text.ParserCombinators.Parsec
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric (readHex)
import Data.Char (chr)


data Expr = Var String | ExprList [Expr] | Lam [String] Expr | Cps [CpsItem] Expr | Let ValDef Expr
data CpsItem = CpsItem [String] Expr

instance Show Expr where
    show (Var name) = name
    show (ExprList exprs) = "(" ++ show' exprs ++ ")"
        where show' [] = ""
              show' [e] = show e
              show' (e : es) = show e ++ " " ++ show' es
    show (Lam names expr) = "\\" ++ show names ++ "." ++ show expr
    show (Cps items finalExprs) = "do" ++ " " ++ show items ++ " > " ++ show finalExprs
    show (Let valDef expr) = "let " ++ show valDef ++ " in " ++ show expr

instance Show CpsItem where
    show (CpsItem names expr) = show names ++ " <- " ++ show expr

data ValDef = ValDef String Expr

instance Show ValDef where
    show (ValDef name expr) = show name ++ " = " ++ show expr

parseEscapeChar :: Parser Char
parseEscapeChar = do
    c <- satisfy (`elem` "\\\"ntvrx")
    case c of
        '\\' -> return '\\'
        '\"' -> return '\"'
        'n' -> return '\n'
        't' -> return '\t'
        'v' -> return '\v'
        'r' -> return '\r'
        'x' -> do
            hexValue <- count 2 (satisfy (`elem` "0123456789abcdefABCDEF"))
            case readHex hexValue of
                (result, _):[] -> return (chr result)
                _ -> error "parse string error"
        _ -> return '?'

parseStringValue :: Parser String
parseStringValue = (skipWhiteCharAndComment *> char '\"' *> parseStringValue')
    where parseStringValue' = do
              value <- many $ satisfy (\c -> not (c == '\\' || c == '\"'))
              c <- satisfy (\c -> c == '\\' || c == '\"')
              case c of
                  '\\' -> do
                      c' <- parseEscapeChar
                      r <- parseStringValue'
                      return (value ++ [c'] ++ r)
                  '\"' -> return value
                  _ -> error "parse string error"


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
parseName = skipWhiteCharAndComment *> many1 (noneOf "λ^.(){}=;< \"\n\r\t")

parseVar :: Parser Expr
parseVar = Var <$> (try parseName <|> try parseStringValue)

parseBracket :: Parser Expr
parseBracket = skipWhiteCharAndComment *> char '(' *> parseExprs <* skipWhiteCharAndComment <* char ')'

parseExpr :: Parser Expr
parseExpr = try parseCps <|> try parseLet <|> try parseVar <|> try parseBracket <|> try parseLam

parseExprs :: Parser Expr
parseExprs = simply . ExprList <$> many1 parseExpr
    where simply (ExprList [expr]) = expr
          simply (ExprList exprs) = ExprList exprs
          simply expr = expr

parseLam :: Parser Expr
parseLam = Lam
    <$> (skipWhiteCharAndComment *> oneOf "λ^" *> many1 (try parseName))
    <*> (skipWhiteCharAndComment *> char '.' *> parseExprs)

parseCps :: Parser Expr
parseCps = Cps <$> (skipWhiteCharAndComment *> char 'c' *> char 'p' *> char 's' *> skipWhiteCharAndComment *> char '{'
    *> many1 (try parseCpsItem)) <*> (parseExprs <* skipWhiteCharAndComment
        <* char '}')

parseCpsItem :: Parser CpsItem
parseCpsItem = CpsItem
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
toLambdaTerm valDefMap (Cps [] finalExprs) = toLambdaTerm valDefMap finalExprs
toLambdaTerm valDefMap (Cps ((CpsItem names expr) : xs) finalExprs) =
    Application (toLambdaTerm valDefMap expr)
        (lambdaFunction names (toLambdaTerm valDefMap (Cps xs finalExprs)))
toLambdaTerm valDefMap (Let (ValDef name val) expr) = Application (Abstraction name (toLambdaTerm valDefMap expr)) (toLambdaTerm valDefMap val)


valDefListToMap :: [ValDef] -> Map String Expr
valDefListToMap valDefList = Map.fromList (map valDefToPair valDefList)
    where valDefToPair (ValDef name expr) = (name, expr)
