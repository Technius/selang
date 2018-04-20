{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Examples:
@
  tagHi :: Term -> TTerm
  tagHi = cata (Fix . Tagged "hi")

  myT :: Term
  myT = Fix (Val (A.NumVal 5))

  complex :: Term
  complex = Fix (Cond myT myT myT)

  tagged :: TTerm
  tagged = tagHi myT

  valInc :: Term -> Term
  valInc (Fix (Val (A.NumVal x))) = Fix (Val (A.NumVal (x + 5)))
  valInc y = y
@

>>> cata (valInc . Fix) complex
Fix (Cond (Fix (Val 10)) (Fix (Val 10)) (Fix (Val 10)))

>>> fold show $ tagHi complex
"Tagged {tag = \"hi\", content = if \"Tagged {tag = \\\"hi\\\", content = 5}\"
then \"Tagged {tag = \\\"hi\\\", content = 5}\" else \"Tagged {tag = \\\"hi\\\",
content = 5}\"}"
-}
module Selang.Ast
  ( Value (..)
  , TermF (..)
  , Term
  , Type (..)
  , FromAst (fromAst)
  , ToAst (toAst)
  ) where

import Data.List (intercalate)
import Data.Functor.Foldable
import Data.Functor.Classes
import Text.Show.Deriving

-- | Constant values
data Value = NumVal Int
           | BoolVal Bool
           | StringVal String
           deriving (Eq)

instance Show Value where
  show (NumVal x) = show x
  show (BoolVal x) = show x
  show (StringVal x) = show x

-- | Typeclass for converting arbitrary `a`s into AST nodes
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

-- | Typeclass for converting arbitrary AST values into Haskell values
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

-- | The AST for the language, meant for use with Fix.
-- | Note that `Term = Fix TermF`
data TermF t = Val Value
             | Ident String
             | Cond t t t
             | Lst [t]
             | FnHost String
             deriving (Functor)

type Term = Fix TermF

instance (Show t) => Show (TermF t) where
  show (Val v) = show v
  show (Ident s) = s
  show (Lst ts) = "[" ++ intercalate ", " (fmap show ts) ++ "]"
  show (Cond cond t f) = "if " ++ show cond ++ " then " ++ show t ++ " else " ++ show f
  show (FnHost name) = "<host function " ++ name ++ ">"

$(deriveShow1 ''TermF)

-- | A sample AST in which each node is tagged with some message
data Tagged f t = Tagged { tag :: String, content :: (f t) } deriving (Show, Functor)
type TTerm = Fix (Tagged TermF)

$(deriveShow1 ''Tagged)

-- | Represents types of values in the language
data Type = TyInt
          | TyBool
          | TyString
          | TyList
          deriving (Eq)

instance Show Type where
  show TyInt = "Int"
  show TyBool = "Bool"
  show TyString = "String"
  show TyList = "List"
