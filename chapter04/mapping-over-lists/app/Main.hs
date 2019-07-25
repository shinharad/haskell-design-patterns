module Main where

import           Control.Applicative

main :: IO ()
main = return ()

-- ----------------------------------------------
doF n = do
  print n
  return (n * 2)

main1 = do
  print $ map (* 2) [2, 3, 5, 7]
  -- [4,6,10,14]
  mapM doF [2, 3, 5, 7] >>= print
  {-
    2
    3
    5
    7
    [4,6,10,14]
  -}
  mapM_ doF [2, 3, 5, 7]
  {-
    2
    3
    5
    7
  -}

-- ----------------------------------------------
mapA :: Applicative f => (a -> f t) -> [a] -> f [t]
mapA f = sequenceA' . map f

sequenceA' :: Applicative f => [f t] -> f [t]
sequenceA' []     = pure []
sequenceA' (x:xs) = (:) <$> x <*> (sequenceA' xs)

-- import Control.Applicative
main2 = mapA doF [2, 3, 5] >>= print

{-
  2
  3
  5
  [4,6,10]
-}
-- ----------------------------------------------
mapA' f []     = pure []
mapA' f (x:xs) = (:) <$> f x <*> (mapA' f xs)

main3 = mapA' doF [2, 3, 5] >>= print
{-
  2
  3
  5
  [4,6,10]
-}
-- ----------------------------------------------
