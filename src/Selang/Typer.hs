module Selang.Typer where

import Data.Map (Map)
import qualified Data.Map as Map
import Selang.Ast

getType :: Map String String -> Term -> Maybe Type
getType ctx (Val v) = Just $ case v of
  NumVal _ -> TyInt
  BoolVal _ -> TyBool
  StringVal _ -> TyString
getType ctx (Ident s) = Nothing -- TODO: Look up identifier type in context
getType ctx (Cond guard branch orelse) =
  case getType ctx guard of
    Just TyBool ->
      let brTy = getType ctx branch
          elTy = getType ctx orelse
       in if brTy == elTy
            then brTy
            else Nothing -- Type mismatch
    Nothing -> Nothing -- Type mismatch
getType ctx (Lst _) = Just TyList
getType ctx (FnHost name) = Nothing -- TODO: look up host type in context
