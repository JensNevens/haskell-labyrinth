
module Utils (shuffle, (-:), retryForever) where

import System.Random

import Control.Monad.Trans.Except

import Data

shuffle :: [a] -> IO [a]
-- shuffle a list
shuffle [] = return []
shuffle xs = do
  randomPos <- getStdRandom (randomR (0, length xs - 1))
  let (left, (a:right)) = splitAt randomPos xs
  fmap (a:) (shuffle (left ++ right))

(-:) :: a -> (a -> b) -> b
-- Is this not in standard Haskell?
x -: f = f x

retryForever :: AskMonad a -> IO a
-- Run an action in the AskMonad
-- If it succeeds, return
-- Else, show the error and retry
retryForever action =
    runExceptT action
    >>= either (\ex -> handle ex >> retryForever action)
               return
  where
    handle ex = putStrLn $ show ex
