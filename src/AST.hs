module AST where

import Data.Text ( Text )

type Name = Text

data Expr
  = Literal Literal
  | Var Name
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | Function Name [Expr] Expr
  deriving (Show)
  
data Literal
  = Int Integer
  | Float Double
  | Bool Bool
  deriving (Show)
