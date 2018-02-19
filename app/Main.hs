module Main where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Megaparsec (runParserT, ParseError)

import Selang.Ast (Term)
import Selang.Parser
import Selang.Eval

main :: IO ()
main = do
  args <- getArgs
  (result, _) <- runStateT (runExceptT $ parseAndEval args) defaultEnv
  case result of
    Left err -> putStrLn err
    Right expr -> print expr

parseAndEval :: [String] -> ExceptT String (StateT Env IO) Term
parseAndEval args = do
  term <- (withExceptT show . ExceptT) $ runParserT parser "<console>" (intercalate " " args)
  res <- withExceptT show (eval term)
  pure res
