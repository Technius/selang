module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Megaparsec (runParserT)

import Selang.Lib

main :: IO ()
main = do
  args <- getArgs
  result <- runParserT parser "<console>" (intercalate " " args)
  case result of
    Left err -> print err
    Right expr -> print expr
