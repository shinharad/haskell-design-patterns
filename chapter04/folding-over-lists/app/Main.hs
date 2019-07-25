module Main where

import           Control.Monad (foldM)
import           Data.Monoid

main :: IO ()
main = return ()

-- ----------------------------------------------
-- The foldl function is tail recursive and strict in the accumulator.
-- The foldr function is non-tail recursive and lazy.
-- In fact, foldl is a special case of foldr
sumLazy []     = 0
sumLazy (x:xs) = x + sumLazy xs

sumLazy' = foldr (+) 0

sumStrict_ acc []     = acc
sumStrict_ acc (x:xs) = sumStrict_ (acc + x) xs

sumStrict = sumStrict_ 0

sumStrict' = foldl (+) 0

main1 = do
  print $ sumLazy [2, 3, 5, 7]
  print $ sumLazy' [2, 3, 5, 7]
  print $ sumStrict [2, 3, 5, 7]
  print $ sumStrict' [2, 3, 5, 7]
    -- 17

-- ----------------------------------------------
-- Folding with monadic functions
doSumStrict :: (Show a, Num a) => a -> [a] -> IO a
doSumStrict acc [] = return acc
doSumStrict acc (x:xs) = do
  putStrLn $ " + " ++ show x ++ " = " ++ show acc'
  doSumStrict acc' xs
  where
    acc' = acc + x

main2 = doSumStrict 0 [2, 3, 5, 7]
  --  + 2 = 2
  --  + 3 = 5
  --  + 5 = 10
  --  + 7 = 17
  --  17

-- To write this as a left-fold, we use the foldM function:
doSumStrict' :: (Show a, Num a) => a -> [a] -> IO a
doSumStrict' = foldM doPlus
  where
    doPlus acc x = do
      putStrLn $ " + " ++ show x ++ " = " ++ show acc'
      return acc'
      where
        acc' = acc + x

main3 = doSumStrict' 0 [2, 3, 5, 7]
  --  + 2 = 2
  --  + 3 = 5
  --  + 5 = 10
  --  + 7 = 17
  --  17

{-
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldl ::            (b -> a -> b)   -> b -> [a] -> b
-}
-- ----------------------------------------------
-- Folding with Monoid
sum' = foldr (+) 0

product' = foldr (*) 1

concatS' = foldr (++) ""

concatL' = foldr (++) []

any' = foldr (||) False

all' = foldr (&&) True

main4 = do
  print $ sum' [2, 3, 5, 7]
  print $ product' [2, 3, 5, 7]
  print $ concatS' ["2", "3", "5", "7"]
  print $ concatL' [["2"], ["3"], ["5"], ["7"]]
  print $ any' [False, False, True, False]
  print $ all' [True, True, True, True]

{-
    17
    210
    "2357"
    ["2","3","5","7"]
    True
    True
-}
-- ----------------------------------------------
newtype Sum' a =
  Sum'
    { getSum' :: a
    }
  deriving (Show)

instance Num a => Semigroup (Sum' a) where
  Sum' x <> Sum' y = Sum' (x + y)

instance Num a => Monoid (Sum' a) where
  mempty = Sum' 0
  Sum' x `mappend` Sum' y = Sum' (x + y)

newtype Product' a =
  Product'
    { getProduct' :: a
    }

instance Num a => Semigroup (Product' a) where
  Product' x <> Product' y = Product' (x * y)

instance Num a => Monoid (Product' a) where
  mempty = Product' 1
  Product' x `mappend` Product' y = Product' (x * y)

-- (defined in Data.Monoid as Sum and Product)
main5 = do
  print $ Sum 10 `mappend` Sum 7
  -- Sum {getSum = 17}
  print $ Product 10 `mappend` Product 7 -- Product {getProduct = 70}

main6 = do
  print $ mconcat [Sum 2, Sum 3, Sum 5, Sum 7]
  -- Sum {getSum = 17}
  print $ mconcat [Product 2, Product 3, Product 5, Product 7]
  -- Product {getProduct = 210}
  print $ mconcat ["2", "3", "5", "7"]
  -- "2357"
  print $ mconcat [["2"], ["3"], ["5"], ["7"]]
  -- ["2","3","5","7"]
  print $ mconcat [Any False, Any False, Any True, Any False]
  -- Any {getAny = True}
  print $ mconcat [All True, All True, All True, All True]
  -- All {getAll = True}
