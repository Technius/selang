{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Selang.Ast
  ( Value
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
