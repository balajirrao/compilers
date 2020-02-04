module Errors where

data CompilerError = TypeCheckError String | UnificationError String deriving Show