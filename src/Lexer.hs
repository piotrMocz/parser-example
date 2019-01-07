module Lexer where

import           Text.Megaparsec            (between, many, notFollowedBy, try)
import           Text.Megaparsec.Char       (alphaNumChar, letterChar, space1,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L

import           Parsers                    (Parser)


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = (lexeme . try) (string w >> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "if", "then" ,"else"
      , "while", "do", "skip"
      , "true", "false"
      , "not", "and", "or"
      ]

identifier :: Parser String
identifier = (lexeme . try) (ident >>= check)
    where
        ident   = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` rws
                  then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                  else return x
