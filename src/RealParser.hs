module RealParser where

import           Control.Applicative
import           Control.Monad.Combinators.Expr (Operator (InfixL, Prefix),
                                                 makeExprParser)
import           Text.Megaparsec                (sepBy1)

import           Ast
import           Lexer
import           Parsers                        (Parser)


-- Statements --

stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi
    where f l = if length l == 1 then head l else Seq l

stmt' :: Parser Stmt
stmt' =  ifStmt
     <|> whileStmt
     <|> skipStmt
     <|> assignStmt
     <|> parens stmt

ifStmt :: Parser Stmt
ifStmt = do
    rword "if"
    cond  <- bExpr
    rword "then"
    stmt1 <- stmt
    rword "else"
    stmt2 <- stmt
    return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt = do
    rword "while"
    cond <- bExpr
    rword "do"
    s    <- stmt
    return $ While cond s

assignStmt :: Parser Stmt
assignStmt = do
    var  <- identifier
    symbol ":="
    expr <- aExpr
    return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip"


-- Expressions --

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
    [ [ Prefix (UMinus <$ symbol "-") ]
    , [ InfixL (ABinary Mul <$ symbol "*")
      , InfixL (ABinary Div <$ symbol "/")
      ]
    , [ InfixL (ABinary Add <$ symbol "+")
      , InfixL (ABinary Sub <$ symbol "-")
      ]
    ]

aTerm :: Parser AExpr
aTerm  =  parens aExpr
      <|> IntVar <$> identifier
      <|> IntLit <$> integer

bTerm :: Parser BExpr
bTerm  =  parens bExpr
      <|> (BoolLit True  <$ rword "true")
      <|> (BoolLit False <$ rword "false")
      <|> rExpr

bOperators :: [[Operator Parser BExpr]]
bOperators =
    [ [ Prefix (BNeg <$ rword "not") ]
    , [ InfixL (BBinary And <$ rword "and")
      , InfixL (BBinary Or  <$ rword "or")
      ]
    ]

rExpr :: Parser BExpr
rExpr = do
    a1 <- aExpr
    op <- relation
    a2 <- aExpr
    return $ BRel op a1 a2

relation :: Parser ARelOp
relation  =  (Greater <$ symbol ">")
         <|> (Less    <$ symbol "<")

