module Selang.Typer where

import Data.Functor.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Selang.Ast

getType :: Map String String -> Term -> Maybe Type
getType ctx (Fix (Val v)) = Just $ case v of
  NumVal _ -> TyInt
  BoolVal _ -> TyBool
  StringVal _ -> TyString
getType ctx (Fix (Ident s)) = Nothing -- TODO: Look up identifier type in context
getType ctx (Fix (Cond guard branch orelse)) =
  case getType ctx guard of
    Just TyBool ->
      let brTy = getType ctx branch
          elTy = getType ctx orelse
       in if brTy == elTy
            then brTy
            else Nothing -- Type mismatch
    Nothing -> Nothing -- Type mismatch
getType ctx (Fix (Lst _)) = Just TyList
getType ctx (Fix (FnHost name)) = Nothing -- TODO: look up host type in context
