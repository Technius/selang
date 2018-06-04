{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import Data.Bifunctor (first)
import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Megaparsec (runParser, ParseError)
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
  result <- runProgram src defaultEnv
  case result of
    Left (ErrParse err) -> putStrLn $ parseErrorPretty' src err
    Left (ErrEval err) -> putStrLn (show err)
    Right expr -> do
      print (pretty expr)
      let typ = getType (Map.empty) expr
      case typ of
        Just typ' -> putStrLn $ "Type: " ++ show typ'
        Nothing -> pure ()

-- | Monad transformer stack used in the parse/evaluation pipeline
type SeProgT a = ExceptT SeError (StateT Env IO) a

-- | Runs the parse and evaluation pipeline on the given input
runProgram :: String -> Env -> IO (Either SeError Term)
runProgram src env = fst <$> (runStateT (runExceptT $ parseAndEval src) env)

-- | Parses the given string and attempts to evaluate it
parseAndEval :: String -> SeProgT Term
parseAndEval s = do
  term <- liftEither $ first ErrParse (runParser parser "<console>" s)
  result <- withExceptT ErrEval (eval term)
  pure result
