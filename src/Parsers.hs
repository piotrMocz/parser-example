module Parsers where

import           Text.Megaparsec      (Parsec)
import qualified Text.Megaparsec.Char as P


type Parser = Parsec String String

openingParen :: Parser Char
openingParen = P.char '('

