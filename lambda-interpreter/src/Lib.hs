{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Generic.Random
import qualified Test.QuickCheck as QC
import GHC.Generics (Generic)
import qualified Data.Map as M

newtype Variable = Variable Int deriving (Eq, Ord)

instance Show Variable where
  show (Variable v) = "v" ++ show v

-- instance Arbitrary Variable where
--   arbitrary = Variable <$> elements [1..2]


data Expr = Var Variable | Const Int | Lam Variable Expr | App Expr Expr deriving (Show, Generic, Eq)

instance Arbitrary Expr where
  arbitrary = g `suchThat` (typeCheck []) `suchThat` (isInt . eval)
    where
      g = frequency [ (10, Var <$> arbitrary)
                         , (10, Const <$> elements [1..100])
                         , (10, Lam <$> arbitrary <*> arbitrary)
                         , (10, App <$> arbitrary <*> arbitrary)]

data Val = Closure (M.Map Variable Val) Variable Expr | IntVal Int deriving (Show, Generic, Eq)pf

eval :: Expr -> Either String Val
eval = go mempty
  where
    go env (Var x) = case (M.lookup x env) of
      Nothing -> Left "No such variable"
      Just t -> pure t
    go _ (Const t) = pure $ IntVal t
    go env (Lam x e) = pure $ Closure env x e
    go env (App e1 e2) = case (go env e1, go env e2) of
      (Right (Closure lexicalScope x e), Right v) -> go (foldl1 M.union [(M.singleton x v), lexicalScope, env]) e
      _ -> Left "Not a function. Can't apply."

isInt :: Either String Val -> Bool
isInt (Right (IntVal _)) = True
isInt _ = False

isLam :: Expr -> Bool
isLam (Lam _ _) = True
isLam _ =  False

isConst :: Expr -> Bool
isConst (Const x) = True
isConst _ = False


depth :: Expr -> Int
depth (Lam _ e) = 1 + depth e
depth (App e1 e2) = 1 + (min (depth e1) (depth e2))
depth _ = 1

wrap :: String
wrap = "module Test where\n main :: IO ()\n main = print $ "

toHaskell :: Expr -> String
toHaskell (Var x) = "" ++ show x ++ ""
toHaskell (Const x) = "" ++ show x ++ ""
toHaskell (App e1 e2) = "((" ++ toHaskell e1  ++ ") " ++ "(" ++ toHaskell e2 ++ "))"
toHaskell (Lam x e) = "(\\" ++ show x ++ "->" ++ "(" ++ toHaskell e ++ "))"

typeCheck :: [Variable] -> Expr -> Bool
typeCheck scope (Const x) = True
typeCheck scope (Var x) = x `elem` scope
typeCheck scope (Lam x e) = typeCheck (x : scope) e
typeCheck scope (App e1 e2) | isLam e1 = typeCheck scope e1 && typeCheck scope e2
typeCheck _ _ = False


