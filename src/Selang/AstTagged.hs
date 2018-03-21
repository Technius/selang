{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Tagged AST.

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
module Selang.AstTagged
  ( TermF (..)
  , Term
  ) where

import Data.List (intercalate)
import Data.Functor.Foldable
import Data.Functor.Classes
import Text.Show.Deriving

import Selang.Ast (FromAst, ToAst)
import qualified Selang.Ast as A

data TermF t = Val A.Value
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

data Tagged f t = Tagged { tag :: String, content :: (f t) } deriving (Show, Functor)
type TTerm = Fix (Tagged TermF)

$(deriveShow1 ''Tagged)
