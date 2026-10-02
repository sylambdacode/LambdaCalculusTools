{-
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
-}

module LambdaParser.UntypedLambdaTokenParser where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec (SourceName, ParseError, SourcePos)
import Numeric (readHex)
import Data.Char (chr)



data Token = KeywordToken String (Maybe SourcePos) | NameToken String (Maybe SourcePos) | SymbolToken String (Maybe SourcePos) | StringToken String (Maybe SourcePos) | EofToken (Maybe SourcePos)


type UntypedLambdaTokenParser = Parsec String ()

instance Show Token where
    show (KeywordToken keyword _) = keyword
    show (NameToken name _) = name
    show (SymbolToken symbol _) = symbol
    show (StringToken v _) = "\"" ++ v ++ "\""
    show (EofToken _) = "EOF"


getTokenValue :: Token -> String
getTokenValue (KeywordToken keyword _) = keyword
getTokenValue (NameToken name _) = name
getTokenValue (SymbolToken symbol _) = symbol
getTokenValue (StringToken v _) = v
getTokenValue (EofToken _) = ""

getTokenPos :: Token -> Maybe SourcePos
getTokenPos (KeywordToken _ maybePos) = maybePos
getTokenPos (NameToken _ maybePos) = maybePos
getTokenPos (StringToken _ maybePos) = maybePos
getTokenPos (SymbolToken _ maybePos) = maybePos
getTokenPos (EofToken maybePos) = maybePos


matchTokenType :: Token -> Token -> Bool
matchTokenType (KeywordToken keyword1 _) (KeywordToken keyword2 _) = keyword1 == keyword2
matchTokenType (NameToken _ _) (NameToken _ _) = True
matchTokenType (SymbolToken symbol1 _) (SymbolToken symbol2 _) = symbol1 == symbol2
matchTokenType (StringToken _ _) (StringToken _ _) = True
matchTokenType (EofToken _) (EofToken _) = True
matchTokenType _ _ = False



parseEscapeChar :: UntypedLambdaTokenParser Char
parseEscapeChar = do
    _ <- try (char '\\')
    c <- oneOf "\\\"ntvrx" <?> "escape char(\\, \", n, t, v, r, x)"
    case c of
        '\\' -> return '\\'
        '\"' -> return '\"'
        'n' -> return '\n'
        't' -> return '\t'
        'v' -> return '\v'
        'r' -> return '\r'
        'x' -> do
            hexValue <- count 8 (satisfy (`elem` "0123456789abcdefABCDEF") <?> "8 hexadecimal chars")
            case readHex hexValue of
                (result, _):[] -> return (chr result)
                _ -> error "parse string error"
        _ -> error "parse string error"

parseString :: UntypedLambdaTokenParser String
parseString = char '\"' *> parseString'
    where anyCharNotStringEnding = parseEscapeChar <|> try (noneOf "\"") <?> "any char not \""
          parseString' = do
              v <- many anyCharNotStringEnding
              _ <- char '\"' <?> "ending string"
              return v


skipComment :: UntypedLambdaTokenParser Char
skipComment = (try (char '{' *> char '-') *> skipComment') <?> "comment"
    where skipComment' =
            (many $ satisfy (\c -> not (c == '-')))
            *> ((char '-'
            *> ((try $ char '}') <|> skipComment')) <?> "\"-}\"")

skipWhiteChar :: UntypedLambdaTokenParser String
skipWhiteChar = (many $ satisfy (`elem` " \n\r\t")) <?> "white chars"

skipWhiteCharAndComment :: UntypedLambdaTokenParser ()
skipWhiteCharAndComment = (skipWhiteChar *> many (skipComment *> skipWhiteChar) >> return ()) <?> "white chars or comments"


parseKeywordOrName :: UntypedLambdaTokenParser String
parseKeywordOrName = (skipWhiteCharAndComment *> many1 (noneOf "λ^.(){}=;< \"\n\r\t")) <?> "keyword or name"

parseToken :: UntypedLambdaTokenParser Token
parseToken = do
    position <- getPosition
    tokenNoPos <-
        (try (do
                name <- parseKeywordOrName
                case name of
                    "cps" -> return (KeywordToken "cps")
                    "let" -> return (KeywordToken  "let")
                    "in" -> return (KeywordToken "in")
                    _ -> return (NameToken name))
        <|> (SymbolToken <$> try (string "λ"))
        <|> (SymbolToken <$> try (string "^"))
        <|> (SymbolToken <$> try (string "."))
        <|> (SymbolToken <$> try (string "("))
        <|> (SymbolToken <$> try (string ")"))
        <|> (SymbolToken <$> try (string "{"))
        <|> (SymbolToken <$> try (string "}"))
        <|> (SymbolToken <$> try (string "="))
        <|> (SymbolToken <$> try (string ";"))
        <|> (SymbolToken <$> try (string "<-"))
        <|> (StringToken <$> try parseString))
    skipWhiteCharAndComment
    return (tokenNoPos (Just position))

parseTokens :: UntypedLambdaTokenParser [Token]
parseTokens = do
    skipWhiteCharAndComment
    tokenList <- many (parseToken) <* skipWhiteCharAndComment <* eof
    eofPos <- getPosition
    return (tokenList ++ [EofToken (Just eofPos)])

runParseTokens :: SourceName -> String -> Either ParseError [Token]
runParseTokens codeName code = parse parseTokens codeName code


