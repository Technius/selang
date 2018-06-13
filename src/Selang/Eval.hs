{-# LANGUAGE FlexibleContexts #-}
{-|
Provides functions to evaluate expression in the language.
-}
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
import Text.Megaparsec (SourcePos, initialPos)

-- | A mapping of variable names to terms
type Env = Map String TTerm

-- | Tags a host function with a source position
hostTag :: TermF TTerm -> TTerm
hostTag = Fix . Tagged (initialPos "<host>")

-- | The default environment
defaultEnv :: Env
defaultEnv = Map.fromList [("hello", hostTag . Val $ toAst "Hello world!")]

-- | Defines a function that can be called from the language
defineHostFn :: (FromAst a) =>
                String ->
                (a -> TTerm) ->
                (String, TTerm -> Either (EvalError, SourcePos) TTerm)
defineHostFn name f = (name, wrapper)
  where wrapper (Fix (Tagged pos (Val t))) =
          case fromAst t of
            Just a -> Right (f a)
            Nothing -> Left (TypeMismatch, pos)
        wrapper _ = Left (TypeMismatch, initialPos "<host>")

-- | A list of host functions
hostFunctions :: Map String (TTerm -> Either (EvalError, SourcePos) TTerm)
hostFunctions = Map.fromList
  [ defineHostFn "plus1" (hostTag . Val . NumVal . (+1))
  , defineHostFn "quote" (hostTag . Val . StringVal . (show :: String -> String))
  ]

-- | Evaluates the given `Term` in the given environment, possibly raising an
-- error
eval :: (MonadState Env m, MonadError (EvalError, SourcePos) m) => TTerm -> m TTerm
eval (Fix (Tagged _ (Cond cond t f))) = do
  cond' <- eval cond
  case unfix cond' of
    Tagged _ (Val (BoolVal x)) ->
      pure $ if x then t else f
    _ -> throwError (TypeMismatch, tag (unfix cond))
eval (Fix (Tagged pos (Ident name))) = do
  env <- get
  case Map.lookup name env of
    Just v -> pure v
    _ -> throwError (UnknownIdent name, pos)
eval (Fix (Tagged _ (Lst [Fix (Tagged pos (Ident name)), arg]))) = do
  case Map.lookup name hostFunctions of
    Just f -> liftEither (f arg)
    Nothing -> throwError (UnknownIdent name, pos)
eval x = pure x
