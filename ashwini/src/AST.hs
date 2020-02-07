{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module AST where

import qualified GHC.Generics as GHC
import Generic.Data (gliftShowsPrec)

import Data.Functor.Classes
import Data.Functor.Foldable

data Pattern = PatternVar String | PatternCons String [String] deriving (Show)

data Branch e = Branch Pattern e deriving (GHC.Generic1, Functor)

instance Show1 Branch where
  liftShowsPrec = gliftShowsPrec

data AST e =
    LowerVar String
  | UpperVar String
  | Case e [Branch e]
  | BinOp Op e e
  | Number Int
  | App e e
  deriving (GHC.Generic1, Functor)

instance Show1 AST where
  liftShowsPrec = gliftShowsPrec

type Op = String

data Constructor = Constructor String [String] deriving (Show)

data Definition = Defn String [String] (Fix AST) | Data String [Constructor] deriving (Show)


