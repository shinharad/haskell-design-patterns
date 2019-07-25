{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Main where

import           Control.Applicative
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable

main :: IO ()
main = return ()

-- ----------------------------------------------
data Tree a
  = Node a (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

-- A Traversable container must also be a Functor and Foldable:
instance Functor Tree where
  fmap f (Leaf x)             = Leaf (f x)
  fmap f (Node x lTree rTree) = (Node (f x) (fmap f lTree) (fmap f rTree))

instance Foldable Tree where
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x lTree rTree) =
    (foldMap toMonoid lTree) `mappend` (toMonoid x) `mappend` (foldMap toMonoid rTree)

instance Traversable Tree
  -- traverse  :: Applicative ma => (a -> ma b) -> mt a -> ma (mt b)
                                                                     where
  traverse g (Leaf x) = Leaf <$> (g x)
  traverse g (Node x ltree rtree) = Node <$> (g x) <*> (traverse g ltree) <*> (traverse g rtree)

tree1 = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

doF n = do
  print n
  return (n * 2)

main1 = traverse doF aTree

{-
2
3
5
7
11
Node' 4 (Leaf' 6) (Node' 10 (Leaf' 14) (Leaf' 22))
-}
{-
traverse
  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
-}
data Tree' a
  = Node' a (Tree' a) (Tree' a)
  | Leaf' a
  deriving (Show, Functor, Foldable, Traversable)

aTree = Node' 2 (Leaf' 3) (Node' 5 (Leaf' 7) (Leaf' 11))

main2 = traverse doF aTree
