{-# LANGUAGE RankNTypes #-}

module Selang.Errors where

import Text.Megaparsec (ParseError, SourcePos)
import Text.Megaparsec.Stream (Token)
import Data.Void

data EvalError = TypeMismatch
               | UnknownIdent String
               deriving (Show)

data SeError = ErrEval EvalError SourcePos
             | ErrParse (ParseError (Token String) Void)
