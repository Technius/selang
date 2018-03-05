module Main where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.List (intercalate)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Megaparsec (runParserT, ParseError)

import Selang.Ast (Term)
import Selang.Parser
import Selang.Eval
import Selang.Typer

main :: IO ()
main = do
  args <- getArgs
  (result, _) <- runStateT (runExceptT $ parseAndEval args) defaultEnv
  case result of
    Left err -> putStrLn err
    Right expr -> do
      print expr
      let typ = getType (Map.empty) expr
      case typ of
        Just typ' -> putStrLn $ "Type: " ++ show typ'
        Nothing -> pure ()

parseAndEval :: [String] -> ExceptT String (StateT Env IO) Term
parseAndEval args = do
  term <- (withExceptT show . ExceptT) $ runParserT parser "<console>" (intercalate " " args)
  res <- withExceptT show (eval term)
  pure res
