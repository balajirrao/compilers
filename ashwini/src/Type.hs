module Type where

newtype TypeVar = TypeVar String deriving (Show, Eq, Ord)


data Type
  = TyBase String
  | TyVar TypeVar
  | TyFn Type Type
  deriving (Show)