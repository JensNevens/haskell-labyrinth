
module Utils (shuffle, (-:)) where

import System.Random

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
