module Main where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.List (intercalate)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Megaparsec (runParserT, ParseError)
import Text.Megaparsec.Error

import Selang.Ast (Term)
import Selang.Parser
import Selang.Eval
import Selang.Typer
import Selang.Errors

main :: IO ()
main = do
  args <- getArgs
  let src = intercalate " " args
  (result, _) <- runStateT (runExceptT $ parseAndEval src) defaultEnv
  case result of
    Left (ErrParse err) -> putStrLn $ parseErrorPretty' src err
    Left (ErrEval err) -> putStrLn (show err)
    Right expr -> do
      print expr
      let typ = getType (Map.empty) expr
      case typ of
        Just typ' -> putStrLn $ "Type: " ++ show typ'
        Nothing -> pure ()

parseAndEval :: String -> ExceptT SeError (StateT Env IO) Term
parseAndEval s = do
  term <- (withExceptT ErrParse) . ExceptT $ runParserT parser "<console>" s
  res <- withExceptT ErrEval $ (eval term)
  pure res
