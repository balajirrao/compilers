module Errors where

data CompilerError = TypeCheckError String | UnificationError deriving Show