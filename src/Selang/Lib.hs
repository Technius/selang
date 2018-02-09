module Selang.Lib
    ( parser
    , Expr
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
          | StringLit String
          | Ident String
          | ListExpr [Expr]
          deriving (Show, Eq)

type ParserT m = ParsecT Void String m

whitespace :: ParserT m ()
whitespace = L.space space1 (L.skipLineComment ";;") empty

lexeme = L.lexeme whitespace

symbol :: String -> ParserT m String
symbol = L.symbol whitespace

digit :: ParserT m Char
digit = digitChar

number :: ParserT m Expr
number = do
  neg <- optional (symbol "-")
  lead <- digit
  rest <- many digitChar
  let sign = case neg of
        Just _ -> -1
        Nothing -> 1
  pure $ NumLit (sign * read (lead : rest))

letter :: ParserT m Char
letter = letterChar

character :: ParserT m Char
character = letter <|> digit <|> (char '\\' >> char '\'') <|> (char '\\' >> char '\"')

string :: ParserT m Expr
string = StringLit <$> between (char '"') (char '"') (many (character <|> char ' '))

boolean :: ParserT m Expr
boolean = (const (BoolLit True) <$> symbol "true") <|>
          (const (BoolLit False) <$> symbol "false")

literal :: ParserT m Expr
literal = Selang.Lib.string <|> number <|> boolean <?> "literal"

identifier :: ParserT m Expr
identifier = do
  h <- letter
  t <- many (letter <|> digit)
  pure (Ident (h:t))

atom :: ParserT m Expr
atom = identifier <|> literal <?> "atom"

list :: ParserT m Expr
list = between (char '(') (char ')') (ListExpr <$> term `sepBy1` whitespace)

term :: ParserT m Expr
term = atom <|> list <?> "term"

parser :: ParserT m Expr
parser = term
