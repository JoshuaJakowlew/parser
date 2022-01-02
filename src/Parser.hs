module Parser where

import Text.Megaparsec
import Control.Monad.Combinators.Expr qualified as Expr

import AST
import Utils
import Lexer qualified as L

unary :: Name -> Parser (Expr -> Expr)
unary name = UnaryOp name <$ L.symbol name

binary :: Name -> Parser (Expr -> Expr -> Expr)
binary name = BinaryOp name <$ L.symbol name

table :: [[Expr.Operator Parser Expr]]
table =
  [ [ Expr.Prefix $ unary "-"
    , Expr.Prefix $ unary "+" ]
  , [ Expr.InfixR $ binary "^" ]
  , [ Expr.InfixL $ binary "*"
    , Expr.InfixL $ binary "/" ]
  , [ Expr.InfixL $ binary "+"
    , Expr.InfixL $ binary "-" ]
  ]

integer :: Parser Expr
integer = Literal . Int <$> L.integer

float :: Parser Expr
float = Literal . Float <$> L.float

var :: Parser Expr
var = Var <$> L.identifier

call :: Parser Expr
call = Call <$> L.identifier
            <*> L.parens (many expr)

function :: Parser Expr
function = Function <$> L.identifier
                    <*> L.parens (many var)
                    <*> (L.symbol "=" *> expr)

expr :: Parser Expr
expr = Expr.makeExprParser term table

term :: Parser Expr
term =  try float
    <|> try integer
    <|> try function
    <|> try call
    <|> try var
    <|> L.parens expr

