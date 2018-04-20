module Selang.Parser
    ( parser
    ) where

import Data.Char
import Data.Void
import Data.Functor.Foldable
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

-- | Parses a single digit
digit :: ParserT m Char
digit = C.digitChar

-- | Parses an integer literal
number :: ParserT m Value
number = toAst <$> (L.decimal :: ParserT m Int)

-- | Parses a single letter
letter :: ParserT m Char
letter = C.letterChar

-- | Parses a single character
character :: ParserT m Char
character = letter <|> digit <|> (C.char '\\' >> C.char '\'') <|> (C.char '\\' >> C.char '\"')

-- | Parses a string literal
string :: ParserT m Value
string = toAst <$> between (C.char '"') (C.char '"') (many (character <|> C.char ' '))

-- | Parses a boolean literal
boolean :: ParserT m Value
boolean = (const (toAst True) <$> symbol "true") <|>
          (const (toAst False) <$> symbol "false")

-- | Parses a string, number, or boolean literal
literal :: ParserT m Term
literal = lexeme $ Fix . Val <$> (Selang.Parser.string <|> number <|> boolean <?> "literal")

-- | Parses a variable
identifier :: ParserT m Term
identifier = do
  h <- letter
  t <- many (letter <|> digit)
  lexeme (pure (Fix (Ident (h:t))))

-- | Parses a literal or a variable
atom :: ParserT m Term
atom = literal <|> identifier <?> "atom"

-- | Parses a list
list :: ParserT m Term
list = lexeme (inBrackets (Fix . Lst <$> term `sepBy` (symbol ",")))
  where inBrackets = between (C.char '[') (C.char ']')

-- | Parses an if-statement
conditional :: ParserT m Term
conditional = do
  symbol "if"
  cond <- term
  symbol "then"
  t <- term
  symbol "else"
  f <- term
  pure (Fix (Cond cond t f))

term :: ParserT m Term
term = (try conditional) <|> atom <|> list <?> "term"

parser :: ParserT m Term
parser = term
