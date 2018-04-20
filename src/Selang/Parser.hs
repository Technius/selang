module Selang.Parser
    ( parser
    ) where

import Data.Char
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Selang.Ast

type ParserT m = ParsecT Void String m

whitespace :: ParserT m ()
whitespace = L.space C.space1 (L.skipLineComment ";;") empty

lexeme = L.lexeme whitespace

symbol :: String -> ParserT m String
symbol = L.symbol whitespace

digit :: ParserT m Char
digit = C.digitChar

number :: ParserT m Value
number = toAst <$> (L.decimal :: ParserT m Int)

letter :: ParserT m Char
letter = C.letterChar

character :: ParserT m Char
character = letter <|> digit <|> (C.char '\\' >> C.char '\'') <|> (C.char '\\' >> C.char '\"')

string :: ParserT m Value
string = toAst <$> between (C.char '"') (C.char '"') (many (character <|> C.char ' '))

boolean :: ParserT m Value
boolean = (const (toAst True) <$> symbol "true") <|>
          (const (toAst False) <$> symbol "false")

literal :: ParserT m Term
literal = lexeme $ Val <$> (Selang.Parser.string <|> number <|> boolean <?> "literal")

identifier :: ParserT m Term
identifier = do
  h <- letter
  t <- many (letter <|> digit)
  lexeme (pure (Ident (h:t)))

atom :: ParserT m Term
atom = literal <|> identifier <?> "atom"

list :: ParserT m Term
list = lexeme $ between (C.char '[') (C.char ']') (Lst <$> term `sepBy` (symbol ","))

conditional :: ParserT m Term
conditional = do
  symbol "if"
  cond <- term
  symbol "then"
  t <- term
  symbol "else"
  f <- term
  pure (Cond cond t f)

term :: ParserT m Term
term = (try conditional) <|> atom <|> list <?> "term"

parser :: ParserT m Term
parser = term
