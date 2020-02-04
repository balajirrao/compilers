module Unify where

import Data.Map (Map, insert, lookup)
import Errors
import Polysemy
import Polysemy.Error
import Polysemy.State
import Type
import Prelude hiding (lookup)

type Bindings = Map TypeVar Type

resolve :: Member (State Bindings) r => Type -> Sem r (Either Type TypeVar)
resolve (TyVar a) = do
  bindings <- get
  let result = lookup a bindings
  case result of
    Nothing -> pure (Right a)
    Just t' -> resolve t'
resolve t = pure (Left t)

unify :: Members [State Bindings, Error CompilerError] r => Type -> Type -> Sem r ()
unify l@(TyVar _) r = do
  rl <- resolve l
  rr <- resolve r
  case rl of
    Right tvar -> bind tvar r
    Left t -> case rr of
      Right tvar -> bind tvar l
      Left t' -> unify t t'
unify l r@(TyVar _) = unify r l
unify (TyFn a b) (TyFn c d) = unify b d >> unify a c
unify (TyBase t1) (TyBase t2)
  | t1 == t2 = return ()
  | otherwise = throw $ UnificationError ("different base types " ++ (show t1) ++ " and " ++ (show t2))
unify t1 t2 = throw $ UnificationError ("can't unify " ++ (show t1) ++ " " ++ (show t2))

bind :: Member (State Bindings) r => TypeVar -> Type -> Sem r ()
bind tv (TyVar tv') | tv == tv' = return ()
bind tv t = do
  bindings <- get
  put (insert tv t bindings)
