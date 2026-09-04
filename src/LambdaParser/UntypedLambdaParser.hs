module LambdaParser.UntypedLambdaParser where

import UntypedLambdaCalculus.LambdaTerm
import LambdaParser.UntypedLambdaTokenParser

import Text.Parsec.Prim
import Text.Parsec (SourceName, ParseError)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Expr = Var String | ExprList [Expr] | Lam [String] Expr | Cps [CpsItem] Expr | Let [ValDef] Expr
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


type UntypedLambdaParser = Parsec [Token] ()

matchToken :: Token -> UntypedLambdaParser Token
matchToken t = tokenPrim showToken nextPos testToken
    where showToken x = show x
          nextPos pos _ [] = pos
          nextPos pos _ (x : _) =
              case getTokenPos x of
                  Just pos' -> pos'
                  Nothing -> pos
          testToken x = if matchTokenType x t then Just x else Nothing

parseKeywordToken :: String -> UntypedLambdaParser Token
parseKeywordToken keyword = matchToken (KeywordToken keyword Nothing) <?> ("keyword \"" ++ keyword ++ "\"")

parseSymbolToken :: String -> UntypedLambdaParser Token
parseSymbolToken symbol = matchToken (SymbolToken symbol Nothing) <?> ("symbol \"" ++ symbol ++ "\"")
parseNameToken :: UntypedLambdaParser Token
parseNameToken = matchToken (NameToken "" Nothing) <?> "name"
parseStringToken :: UntypedLambdaParser Token
parseStringToken = matchToken (StringToken "" Nothing) <?> "string"

eofToken :: UntypedLambdaParser Token
eofToken = matchToken (EofToken Nothing)

parseNameTokenString :: UntypedLambdaParser String
parseNameTokenString = getTokenValue <$> parseNameToken

parseVar :: UntypedLambdaParser Expr
parseVar = Var <$> (try parseNameTokenString <|> try (getTokenValue <$> parseStringToken))

parseBracket :: UntypedLambdaParser Expr
parseBracket = try (parseSymbolToken "(") *> parseExprs <* (parseSymbolToken ")")

parseExpr :: UntypedLambdaParser Expr
parseExpr = parseCps <|> parseLet <|> try parseVar <|> parseBracket <|> parseLam <?> "cps expression, let expression, (expression), lambda abstraction expression"

parseExprs :: UntypedLambdaParser Expr
parseExprs = simply . ExprList <$> (many1 parseExpr <?> "expressions")
    where simply (ExprList [expr]) = expr
          simply (ExprList exprs) = ExprList exprs
          simply expr = expr

parseLam :: UntypedLambdaParser Expr
parseLam = Lam
    <$> (try (try (parseSymbolToken "λ") <|> parseSymbolToken "^") *> many1 parseNameTokenString)
    <*> (((parseSymbolToken ".") <?> ". expressions") *> parseExprs)

parseCps :: UntypedLambdaParser Expr
parseCps = Cps <$> (try (parseKeywordToken "cps") *> parseSymbolToken "{"
    *> many1 (parseCpsItem <?> "variables <- expresssion;")) <*> ((parseExprs <?> "expression") <* ((parseSymbolToken "}") <?> "\"}\""))

parseCpsItem :: UntypedLambdaParser CpsItem
parseCpsItem = CpsItem
    <$> try (many1 parseNameTokenString <* parseSymbolToken "<-")
    <*> parseExprs <* parseSymbolToken ";"

parseLet :: UntypedLambdaParser Expr
parseLet = Let
    <$> (try ((parseKeywordToken "let")) *> (many1 (parseValDef)))
    <*> (parseKeywordToken "in" *> parseExprs)

parseValDef :: UntypedLambdaParser ValDef
parseValDef = ValDef
    <$> ((try parseNameTokenString) <?> "name (name = epxression;)")
    <*> ((parseSymbolToken "=") *> parseExprs <* (parseSymbolToken ";" <?> "\";\""))

parseValDefs :: UntypedLambdaParser [ValDef]
parseValDefs = (many parseValDef)

parseCode :: UntypedLambdaParser [ValDef]
parseCode = parseValDefs <* eofToken

runParseCode :: SourceName -> String -> Either ParseError [ValDef]
runParseCode codeName code =
    case runParseTokens codeName code of
        Right tokenList -> parse parseCode codeName tokenList
        Left e -> Left e

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

handleLetExpression :: Expr -> Expr
handleLetExpression (Let (valDef : []) expr) = Let (valDef : []) expr
handleLetExpression (Let (valDef : valDefs) expr) = Let (valDef : []) (Let valDefs expr)
handleLetExpression (Let [] _) = error "error let expression"
handleLetExpression _ = error "not let expression"

toLetLambdaTerm :: Set String -> Map String Expr -> Expr -> LambdaTerm
toLetLambdaTerm varSet valDefMap (Let ((ValDef name val) : []) expr) =
    Application bodyLambdaTerm argLambdaTerm
    where bodyLambdaTerm = Abstraction name (toLambdaTerm (Set.insert name varSet) valDefMap expr)
          argLambdaTerm = toLambdaTerm varSet valDefMap val
toLetLambdaTerm _ _ _ = error "error let expression"

toLambdaTerm :: Set String -> Map String Expr -> Expr -> LambdaTerm
toLambdaTerm varSet valDefMap (Var name) =
    if not (name `Set.member` varSet)
        then
            case Map.lookup name valDefMap of
            Just expr -> toLambdaTerm varSet valDefMap expr
            Nothing -> Variable name
        else Variable name
toLambdaTerm varSet valDefMap (ExprList exprs) = lambdaTermList (map (toLambdaTerm varSet valDefMap) exprs)
toLambdaTerm varSet valDefMap (Lam names expr) = lambdaFunction names (toLambdaTerm (Set.union varSet (Set.fromList names)) valDefMap expr)
toLambdaTerm varSet valDefMap (Cps [] finalExprs) = toLambdaTerm varSet valDefMap finalExprs
toLambdaTerm varSet valDefMap (Cps ((CpsItem names expr) : xs) finalExprs) =
    Application (toLambdaTerm varSet valDefMap expr)
        (lambdaFunction names (toLambdaTerm varSet valDefMap (Cps xs finalExprs)))
toLambdaTerm varSet valDefMap (Let valDefs expr) = toLetLambdaTerm varSet valDefMap handledLetExpression
    where handledLetExpression = handleLetExpression (Let valDefs expr)

valDefListToMap :: [ValDef] -> Map String Expr
valDefListToMap valDefList = Map.fromList (map valDefToPair valDefList)
    where valDefToPair (ValDef name expr) = (name, expr)
