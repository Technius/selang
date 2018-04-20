{-# LANGUAGE FlexibleContexts #-}
module Selang.Eval
  ( eval
  , defaultEnv
  , Env
  , EvalError
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Foldable
import Data.Map as Map
import Selang.Ast
import Selang.Errors

type Env = Map String Term

defaultEnv :: Env
defaultEnv = Map.fromList [("hello", Fix . Val $ toAst "Hello world!")]

defineHostFn :: (FromAst a) => String -> (a -> Term) -> (String, Term -> Either EvalError Term)
defineHostFn name f = (name, wrapper)
  where wrapper (Fix (Val t)) =
          case fromAst t of
            Just a -> Right (f a)
            Nothing -> Left TypeMismatch
        wrapper _ = Left TypeMismatch

hostFunctions :: Map String (Term -> Either EvalError Term)
hostFunctions = Map.fromList
  [ defineHostFn "plus1" (Fix . Val . NumVal . (+1))
  , defineHostFn "quote" (Fix . Val . StringVal . (show :: String -> String))
  ]

eval :: (MonadState Env m, MonadError EvalError m) => Term -> m Term
eval (Fix (Cond cond t f)) = do
  cond' <- eval cond
  case unfix cond' of
    Val (BoolVal x) ->
      pure $ if x then t else f
    _ -> throwError TypeMismatch
eval (Fix (Ident name)) = do
  env <- get
  case Map.lookup name env of
    Just v -> pure v
    _ -> throwError (UnknownIdent name)
eval (Fix (Lst [Fix (Ident name), arg])) = do
  case Map.lookup name hostFunctions of
    Just f -> liftEither (f arg)
    Nothing -> throwError (UnknownIdent name)
eval x = pure x
