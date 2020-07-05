module Main where

import Lexer
import Parser
import TypeCheck
import PPrint hiding (print)
-- import AST
import TIM

import Polysemy.Reader
import Polysemy
import Polysemy.Internal()

import Control.Monad(forM)

import Data.List (unfoldr)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main = do
    text <- getContents
    let prog = parse (scanTokens text)
        (bindings, env) = typecheckProg prog

    T.putStrLn . (T.unlines . (T.unlines <$>)) $ run . (runReader 0) $ forM prog pprintDefinition
    T.putStrLn . T.unlines $ run . runReader bindings $ pprintTypeEnv env

    let states = unfoldr step' (compile prog)
    print states
    return ()
        where
            step' :: MachineState -> Maybe (MachineState, MachineState)
            step' m | isFinal m = Nothing
            step' m = Just (m, step m)