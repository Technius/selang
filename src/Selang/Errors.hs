{-# LANGUAGE RankNTypes #-}

module Selang.Errors where

import Text.Megaparsec (ParseError)
import Text.Megaparsec.Stream (Token)
import Data.Void

data EvalError = TypeMismatch
               | UnknownIdent String
               deriving (Show)

data SeError = ErrEval EvalError
             | ErrParse (ParseError (Token String) Void)
