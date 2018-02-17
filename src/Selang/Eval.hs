module Selang.Eval
  ( eval
  , EvalError
  , EvalResult
  ) where

import Control.Monad.Trans.Except
import Selang.Ast

data EvalError = TypeMismatch deriving (Show)

type EvalResult m = ExceptT EvalError m Term

eval :: (Monad m) => Term -> EvalResult m
eval (Cond cond t f) = do
  cond' <- eval cond
  case cond' of
    Val (BoolVal x) ->
      pure $ if x then t else f
    _ -> throwE TypeMismatch
eval x = pure x
