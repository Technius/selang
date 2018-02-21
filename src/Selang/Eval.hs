module Selang.Eval
  ( eval
  , defaultEnv
  , Env
  , EvalError
  , EvalResult
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Map as Map
import Selang.Ast

data EvalError = TypeMismatch
               | UnknownIdent String
               deriving (Show)

type EvalResult m = ExceptT EvalError (StateT Env m) Term

type Env = Map String Term

defaultEnv :: Env
defaultEnv = Map.fromList [("hello", Val $ toAst "Hello world!")]

defineHostFn :: (FromAst a) => String -> (a -> Term) -> (String, Term -> Either EvalError Term)
defineHostFn name f = (name, wrapper)
  where wrapper (Val t) =
          case fromAst t of
            Just a -> Right (f a)
            Nothing -> Left TypeMismatch
        wrapper _ = Left TypeMismatch

hostFunctions :: Map String (Term -> Either EvalError Term)
hostFunctions = Map.fromList
  [ defineHostFn "plus1" (Val . NumVal . (+1))
  , defineHostFn "quote" (Val . StringVal . (show :: String -> String))
  ]

eval :: (Monad m) => Term -> EvalResult m
eval (Cond cond t f) = do
  cond' <- eval cond
  case cond' of
    Val (BoolVal x) ->
      pure $ if x then t else f
    _ -> throwE TypeMismatch
eval (Ident name) = do
  env <- lift get
  case Map.lookup name env of
    Just v -> pure v
    _ -> throwE (UnknownIdent name)
eval (Lst [Ident name, arg]) = do
  case Map.lookup name hostFunctions of
    Just f -> ExceptT (pure (f arg))
    Nothing -> throwE (UnknownIdent name)
eval x = pure x
