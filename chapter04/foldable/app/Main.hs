module Main where

import qualified Control.Monad as M
import qualified Data.Foldable as F
import           Data.Monoid

main :: IO ()
main = return ()

-- ----------------------------------------------
data Tree a
  = Node a (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

foldT :: Monoid a => Tree a -> a
foldT (Leaf x)             = x
foldT (Node x lTree rTree) = (foldT lTree) `mappend` x `mappend` (foldT rTree)

main1 = do
  print . foldT $ Node (Sum 2) (Leaf (Sum 3)) (Leaf (Sum 5))
  -- Sum {getSum = 10}
  print . foldT $ Node (Product 2) (Leaf (Product 3)) (Leaf (Product 5))
  -- Product {getProduct = 30}

-- ----------------------------------------------
foldT' :: Monoid a => (t -> a) -> Tree t -> a
foldT' toMonoid (Leaf x) = toMonoid x
foldT' toMonoid (Node x lTree rTree) = (foldT' toMonoid lTree) `mappend` (toMonoid x) `mappend` (foldT' toMonoid rTree)

main2 = do
  print $ foldT' Sum aTree
  -- Sum {getSum = 10}
  print $ foldT' Product aTree
  -- Product {getProduct = 30}
  print $ foldT' (Any . (== 5)) aTree
  -- Any {getAny = True}
  print $ foldT' (All . (> 0)) aTree
  -- All {getAll = True}
  where
    aTree = Node 2 (Leaf 3) (Leaf 5)

-- ----------------------------------------------
-- Foldable
instance F.Foldable Tree where
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x lTree rTree) =
    (F.foldMap toMonoid lTree) `mappend` (toMonoid x) `mappend` (F.foldMap toMonoid rTree)

main3 = do
  print $ F.foldMap Sum aTree
  -- Sum {getSum = 10}
  print $ F.foldMap Product aTree
  -- Product {getProduct = 30}
  print $ F.foldMap (Any . (== 5)) aTree
  -- Any {getAny = True}
  print $ F.foldMap (All . (> 0)) aTree
  -- All {getAll = True}
  where
    aTree = Node 2 (Leaf 3) (Leaf 5)

-- ----------------------------------------------
main4 = do
  print $ F.sum aTree
  -- 10
  print $ F.product aTree
  -- 30
  print $ F.any (== 5) aTree
  -- True
  print $ F.all (> 0) aTree
  -- True
  print $ F.maximum aTree
  -- 5
  where
    aTree = Node 2 (Leaf 3) (Leaf 5)

doSum = F.foldrM doPlus
  where
    doPlus acc x = do
      putStrLn $ " + " ++ show x ++ " = " ++ show acc
      return (acc + x)

main5 = doSum 0 aTree
  where
    aTree = Node 2 (Leaf 3) (Leaf 5)

{-
 + 0 = 5
 + 5 = 2
 + 7 = 3
10
-}
-- ----------------------------------------------
main6 = do
  print $ F.toList aTree
  -- [3,2,5]
  print $ F.foldr (:) [] aTree
  -- [3,2,5]
  where
    aTree = Node 2 (Leaf 3) (Leaf 5)
