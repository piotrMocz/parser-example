module Parsers where

import           Text.Megaparsec      (Parsec, (<|>))
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P


type Parser      = Parsec String String
type ParserError = P.ParseErrorBundle String String

parse :: Parser a -> String -> Either ParserError a
parse parser input = P.parse parser "" input

openingParen :: Parser Char
openingParen = P.char '('

closingParen :: Parser Char
closingParen = P.char ')'

paren :: Parser Char
paren = openingParen <|> closingParen

emptyParens :: Parser Char
emptyParens = openingParen >> closingParen
