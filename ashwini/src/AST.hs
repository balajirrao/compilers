module AST where

data N = NLowerVar String | NUpperVar String [String] deriving Show

data R = R0 N Add deriving Show

data LB = LBR R LB | LB0 R deriving Show

data C = Case Add LB deriving Show

data Base = BNumber Int | LowerVar String | UpperVar String | BaseAdd Add | Base0 C deriving Show

data App = AppR App Base | App0 Base deriving Show

data Mul = MTimes Mul App | MDivide Mul App | Mul0 App deriving Show

data Add = APlus Add Mul | AMinus Add Mul | Add0 Mul deriving Show

data Constructor = Cons String [String] deriving Show

data Definition = Defn String [String] Add | Data String [Constructor] deriving Show

-- data AST
--   = Program [Definition]
--   | Definition Definition
--   | Constructor Constructor
--   | Add Add
--   | Mul Mul
--   | App App
--   | Base Base
--   | C C
--   | LB LB
--   | R R
--   | N N
--   deriving Show