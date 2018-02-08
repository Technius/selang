module Selang.Lib
    ( someFunc
    ) where

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

someFunc :: IO ()
someFunc = putStrLn "Hello world!"

data Expr = NumLit Int
          | BoolLit Bool
          deriving (Show, Eq)

type ParserT m = ParsecT Void String m

whitespace :: ParserT m ()
whitespace = L.space space1 (L.skipLineComment ";;") empty

lexeme = L.lexeme whitespace

symbol :: String -> ParserT m String
symbol = L.symbol whitespace

number :: ParserT m Expr
number = do
  neg <- optional (symbol "-")
  lead <- digitChar
  rest <- many digitChar
  let sign = case neg of
        Just _ -> -1
        Nothing -> 1
  pure $ NumLit (sign * read (lead : rest))

boolean :: ParserT m Expr
boolean = (const (BoolLit True) <$> symbol "true") <|> (const (BoolLit False) <$> symbol "false")

literal :: ParserT m Expr
literal = number <|> boolean
