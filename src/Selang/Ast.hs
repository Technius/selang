{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Selang.Ast
  ( Value (..)
  , Term (..)
  , Type (..)
  , ToAst (toAst)
  , FromAst (fromAst)
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
          | FnHost String
          deriving (Eq)

data Type = TyInt
          | TyBool
          | TyString
          | TyList
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
  show (FnHost name) = "<host function " ++ name ++ ">"

instance Show Type where
  show TyInt = "Int"
  show TyBool = "Bool"
  show TyString = "String"
  show TyList = "List"

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

class FromAst a where
  fromAst :: Value -> Maybe a

instance FromAst Int where
  fromAst (NumVal x) = Just x
  fromAst _ = Nothing

instance FromAst String where
  fromAst (StringVal x) = Just x
  fromAst _ = Nothing

instance FromAst Bool where
  fromAst (BoolVal x) = Just x
  fromAst _ = Nothing
