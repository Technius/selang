{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Selang.Ast
  ( Value (..)
  , Term (..)
  , ToAst (toAst)
  ) where

import Data.List (intercalate)

data Value = NumVal Int
           | BoolVal Bool
           | StringVal String
           deriving (Eq)

data Term = Val Value
          | Ident String
          | Cond Term Term Term
          | Lst [Term]
          deriving (Eq)

instance Show Value where
  show (NumVal x) = show x
  show (BoolVal x) = show x
  show (StringVal x) = show x

instance Show Term where
  show (Val v) = show v
  show (Ident s) = s
  show (Lst ts) = "[" ++ intercalate ", " (fmap show ts) ++ "]"
  show (Cond cond t f) = "if " ++ show cond ++ " then " ++ show t ++ " else " ++ show f

class ToAst a b where
  toAst :: a -> b

instance ToAst Integer Value where
  toAst = NumVal . fromInteger

instance ToAst Int Value where
  toAst = NumVal

instance ToAst String Value where
  toAst = StringVal

instance ToAst Bool Value where
  toAst = BoolVal
