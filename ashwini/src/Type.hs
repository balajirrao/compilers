module Type where

import qualified Data.Map as M

newtype TypeEnv = TypeEnv (M.Map String Type)
newtype TypeVar = TypeVar String deriving (Show, Eq, Ord)

data Type
  = TyBase String
  | TyVar TypeVar
  | TyFn Type Type
  deriving (Show)