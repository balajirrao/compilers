module Main where

import Generic.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Process.Typed
import GHC.IO.Handle

import Data.ByteString.Lazy.Char8 hiding (hPutStr, putStrLn)

import Control.Monad.IO.Class

import Lib

main :: IO ()
main = do
  xs <- (sample' (arbitrary :: Gen Expr))
  print xs
  -- print $ toHaskell expr
  -- quickCheck correct

correct :: Expr -> Property
correct expr = monadicIO $ do
  res <- liftIO $ -- withProcess_ (setStdin createPipe $ setStdout createPipe $ proc "stack" ["ghci"]) $ \p -> do
    print expr
      -- hPutStr (getStdin p) (toHaskell expr)
      -- read <$> (hGetLine $ getStdout p)
  assert $ 10 == 10
