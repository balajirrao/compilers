module TIM where

import Data.Functor.Classes
import Data.Functor.Foldable

import qualified Data.Map as M

import AST

type Addr = Int

type Stack = [Addr]

data Node = NAp Addr Addr | NSupercomb String [String] (Fix AST) | NNum Int deriving Show

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False

type Globals = M.Map String Addr

addGlobal :: String -> Addr -> Globals -> Globals
addGlobal = M.insert

type Heap = (Addr, M.Map Addr Node)

emptyHeap :: Heap
emptyHeap = (0, M.empty)

hAlloc :: Heap -> Node -> (Heap, Addr)
hAlloc (maxAddr, h) n = ((maxAddr + 1, M.insert maxAddr n h), maxAddr)

hLookup :: Heap -> Addr -> Node
hLookup (_, h) a = h M.! a

data MachineState
    = MachineState
    {
        stack :: Stack
      , heap :: Heap
      , globals :: Globals
} deriving Show

compile :: [Definition] -> MachineState
compile defs = MachineState s h g
    where
        (h, g) = buildInitialHeap defs
        s = [g M.! "main"]

buildInitialHeap :: [Definition] -> (Heap, Globals)
buildInitialHeap = foldr go (emptyHeap, M.empty)
    where
        go (Defn f args body) (h, g) =
            let (h', addr) = hAlloc h (NSupercomb f args body)
            in (h', addGlobal f addr g)

isFinal :: MachineState -> Bool
isFinal (MachineState [addr] h _) | isDataNode (hLookup h addr) = True
isFinal (MachineState [] _ _) = error "Empty stack!"
isFinal _ = False

step :: MachineState -> MachineState
step m@(MachineState st@(x:_) h g) = dispatch (hLookup h x)
    where
        dispatch :: Node -> MachineState
        dispatch (NNum _) = error "Can't apply number!"
        dispatch (NAp a1 _) = MachineState (a1 : st) h g
        dispatch (NSupercomb _ args body) = m { stack = newStack
                                              , heap = newHeap}
            where
                newStack = resultAddr : (drop (1 + length args) st)
                (newHeap, resultAddr) = instantiate body h env
                env = M.fromList $ zip args (getArgs h st)

getArgs :: Heap -> Stack -> [Addr]
getArgs h (_:s) = getArg <$> s
    where
        getArg addr | (NAp _ _) <- hLookup h addr = addr
        getArg _ = error "Not an NAp node in the stack"

instantiateAST :: AST (Heap -> M.Map String Addr -> (Heap, Addr)) -> Heap -> M.Map String Addr -> (Heap, Addr)
instantiateAST (Number n) h _ = hAlloc h (NNum n)
instantiateAST (App e1 e2) h env = let (heap1, a1) = e1 h env
                                       (heap2, a2) = e2 heap1 env
                                in hAlloc heap2 (NAp a1 a2)
instantiateAST (LowerVar x) h env = (h, env M.! x)
instantiateAST _ _ _ = error ("Can not instantiate")

instantiate :: Fix AST -> Heap -> M.Map String Addr -> (Heap, Addr)
instantiate = fold instantiateAST
