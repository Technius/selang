{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.List (intercalate)
import Data.Bifunctor (first)
import Data.Text.Prettyprint.Doc (pretty)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Megaparsec (runParser, ParseError, sourcePosPretty)
import Text.Megaparsec.Error

import Selang.Ast (Term, TTerm, untag)
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
    Left (ErrEval err pos) ->
      putStrLn ("Error at " ++ sourcePosPretty pos ++ ": " ++ show err)
    Right expr -> do
      let untagged = untag expr
      print (pretty untagged)
      let typ = getType Map.empty untagged
      case typ of
        Just typ' -> putStrLn $ "Type: " ++ show typ'
        Nothing -> pure ()

-- | Monad transformer stack used in the parse/evaluation pipeline
type SeProgT a = ExceptT SeError (StateT Env IO) a

-- | Runs the parse and evaluation pipeline on the given input
runProgram :: String -> Env -> IO (Either SeError TTerm)
runProgram src env = fst <$> (runStateT (runExceptT $ parseAndEval src) env)

-- | Parses the given string and attempts to evaluate it
parseAndEval :: String -> SeProgT TTerm
parseAndEval s = do
  term <- liftEither $ first ErrParse (runParser parser "<console>" s)
  result <- withExceptT (uncurry ErrEval) (eval term)
  pure result
