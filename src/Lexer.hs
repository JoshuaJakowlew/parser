module Lexer where

import Data.Functor
import Data.Void ( Void )
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import AST qualified
import Utils

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

identifier :: Parser T.Text
identifier = lexeme $ T.pack <$> str
  where
    str = (:) <$> lowerChar
              <*> many alphaNumChar
