module Main where

import Lexer
import Parser
import TypeCheck
import PPrint

import Polysemy.Reader
import Polysemy
import Polysemy.Internal

import Control.Monad(forM)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

compile :: String -> IO ()
compile progText = do
    let prog = parse (scanTokens progText)
        (bindings, env) = typecheckProg prog

    T.putStrLn . (T.unlines . (T.unlines <$>)) $ run . (runReader 0) $ forM prog pprintDefinition
    T.putStrLn . T.unlines $ run . runReader bindings $ pprintTypeEnv env

main :: IO ()
main = do
    getContents >>= compile