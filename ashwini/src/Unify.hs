module Unify where

import Prelude hiding (lookup)

import Data.Map (Map, lookup, insert)

import Polysemy
import Polysemy.State
import Polysemy.Error

import Type

import Errors

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
unify (TyFn a b) (TyFn c d) = unify a c >> unify b d
unify (TyBase t1) (TyBase t2) | t1 == t2 = return ()
                              | otherwise = throw UnificationError
unify _ _ = throw UnificationError

bind :: Member (State Bindings) r => TypeVar -> Type -> Sem r ()
bind tv t@(TyVar tv') | tv == tv' = return ()
                      | otherwise = do
                          bindings <- get
                          put (insert tv t bindings)
bind _ _ = return ()