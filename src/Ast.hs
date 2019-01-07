module Ast where

data AExpr = IntVar  String
           | IntLit  Integer
           | UMinus  AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data BExpr = BoolLit Bool
           | BNeg    BExpr
           | BBinary BBinOp BExpr BExpr
           | BRel    ARelOp AExpr AExpr
           deriving (Show)

data ABinOp = Add | Sub | Mul | Div deriving (Show)

data BBinOp = And | Or deriving (Show)

data ARelOp = Less | Greater deriving (Show)

data Stmt = Assign String AExpr
          | Skip
          | Seq   [Stmt]
          | If    BExpr Stmt Stmt
          | While BExpr Stmt
          deriving (Show)
