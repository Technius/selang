module Selang.Lib
    ( parser
    , Term
    ) where

import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Value = NumVal Int
           | BoolVal Bool
           | StringVal String
           deriving (Show, Eq)

data Term = Val Value
          | Ident String
          | Lst [Term]
          deriving (Show, Eq)

type ParserT m = ParsecT Void String m

whitespace :: ParserT m ()
whitespace = L.space space1 (L.skipLineComment ";;") empty

lexeme = L.lexeme whitespace

symbol :: String -> ParserT m String
symbol = L.symbol whitespace

digit :: ParserT m Char
digit = digitChar

number :: ParserT m Value
number = do
  neg <- optional (symbol "-")
  lead <- digit
  rest <- many digitChar
  let sign = case neg of
        Just _ -> -1
        Nothing -> 1
  pure $ NumVal (sign * read (lead : rest))

letter :: ParserT m Char
letter = letterChar

character :: ParserT m Char
character = letter <|> digit <|> (char '\\' >> char '\'') <|> (char '\\' >> char '\"')

string :: ParserT m Value
string = StringVal <$> between (char '"') (char '"') (many (character <|> char ' '))

boolean :: ParserT m Value
boolean = (const (BoolVal True) <$> symbol "true") <|>
          (const (BoolVal False) <$> symbol "false")

literal :: ParserT m Term
literal = Val <$> (Selang.Lib.string <|> number <|> boolean <?> "literal")

identifier :: ParserT m Term
identifier = do
  h <- letter
  t <- many (letter <|> digit)
  pure (Ident (h:t))

atom :: ParserT m Term
atom = identifier <|> literal <?> "atom"

list :: ParserT m Term
list = between (char '(') (char ')') (Lst <$> term `sepBy1` whitespace)

term :: ParserT m Term
term = atom <|> list <?> "term"

parser :: ParserT m Term
parser = term
