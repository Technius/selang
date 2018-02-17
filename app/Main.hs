module Main where

import Control.Monad.Trans.Except
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Megaparsec (runParserT, ParseError)

import Selang.Ast (Term)
import Selang.Parser
import Selang.Eval

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT $ parseAndEval args
  case result of
    Left err -> putStrLn err
    Right expr -> print expr

parseAndEval :: [String] -> ExceptT String IO Term
parseAndEval args = do
  term <- (withExceptT show . ExceptT) $ runParserT parser "<console>" (intercalate " " args)
  res <- withExceptT show (eval term)
  pure res
