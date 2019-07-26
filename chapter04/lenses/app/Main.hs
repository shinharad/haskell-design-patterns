module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid

main :: IO ()
main = return ()

-- ----------------------------------------------
data Tree a
  = Node a (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

intTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

listTree = Node [1, 1] (Leaf [2, 1]) (Node [3, 2] (Leaf [5, 2]) (Leaf [7, 4]))

tupleTree = Node (1, 1) (Leaf (2, 1)) (Node (3, 2) (Leaf (5, 2)) (Leaf (7, 4)))

-- ----------------------------------------------
getRoot :: Tree a -> a
getRoot (Leaf z)     = z
getRoot (Node z _ _) = z

setRoot :: Tree a -> a -> Tree a
setRoot (Leaf z) x     = Leaf x
setRoot (Node z l r) x = Node x l r

main1 = do
  print $ getRoot intTree
  -- 2
  print $ setRoot intTree 11
  -- Node 11 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
  print $ getRoot (setRoot intTree 11)
  -- 11

-- ----------------------------------------------
fmapRoot :: (a -> a) -> Tree a -> Tree a
fmapRoot f tree = setRoot tree newRoot
  where
    newRoot = f (getRoot tree)

fmapRoot' :: (a -> a) -> Tree a -> Tree a
fmapRoot' f (Leaf z)     = Leaf (f z)
fmapRoot' f (Node z l r) = Node (f z) l r

setRoot' :: Tree a -> a -> Tree a
setRoot' tree x = fmapRoot' (\_ -> x) tree

main2 = do
  print $ setRoot' intTree 11
  -- Node 11 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
  print $ fmapRoot' (* 2) intTree
  -- Node 4  (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

{-
  fmapRoot'  :: (a -> a)    -> Tree a ->     Tree a

  To make provision for IO we need to define the following function:
  fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)

  We can generalize this beyond IO to all monads:
  fmapM      :: (a -> m a)  -> Tree a -> m  (Tree a)

  type Lens' s a
     = Functor f' =>
         (a -> f' a) -> s -> f' s
-}
-- ----------------------------------------------
-- Writing a Lens
{-
  lens'      :: Functor f  => (a -> f' a) -> s      -> f' s
  root       :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)
  fmapRootIO ::               (a -> IO a) -> Tree a -> IO (Tree a)
-}
-- This is still not very tangible; fmapRootIO is easier to understand, with the Functor f' being IO:
fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)
fmapRootIO g (Leaf z)     = (g z) >>= return . Leaf
fmapRootIO g (Node z l r) = (g z) >>= return . (\x -> Node x l r)

displayM :: Show a => a -> IO a
displayM x = print x >> return x

main3 = fmapRootIO displayM intTree

{-
  2
  Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
-}
-- ----------------------------------------------
-- If we drop down from Monad into Functor, we have a Lens for the root of a Tree:
root :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)
root g (Node z l r) = fmap (\x -> Node x l r) (g z)
root g (Leaf z)     = fmap (\x -> Leaf x) (g z)

main4 = root displayM intTree

{-
  2
  Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
-}
main5 = do
  print $ view root listTree
  -- [1,1]
  print $ view root intTree
  -- 2
  print $ set root [42] listTree
  -- Node [42] (Leaf [2,1]) (Node [3,2] (Leaf [5,2]) (Leaf [7,4]))
  print $ set root 42 intTree
  -- Node 42 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
  print $ over root (+ 11) intTree -- Node 13 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

-- ----------------------------------------------
-- Composable getters and setters
rightMost :: Functor f' => (a -> f' a) -> Tree a -> f' (Tree a)
rightMost g (Node z l r) = fmap (\r' -> Node z l r') (rightMost g r)
rightMost g (Leaf z)     = fmap (\x -> Leaf x) (g z)

{-
  tupleTree = Node (1, 1) (Leaf (2, 1)) (Node (3, 2) (Leaf (5, 2)) (Leaf (7, 4)))
-}
main6 --2
 = do
  print $ view rightMost tupleTree
  -- (7,4)
  print $ set rightMost (0, 0) tupleTree
  -- Node (1,1) (Leaf (2,1)) (Node (3,2) (Leaf (5,2)) (Leaf (0,0)))
  print $ view (rightMost . _1) tupleTree
  -- 7
  print $ set (rightMost . _1) 0 tupleTree
  -- Node (1,1) (Leaf (2,1)) (Node (3,2) (Leaf (5,2)) (Leaf (0,4)))
  print $ over (rightMost . _1) (* 100) tupleTree -- Node (1,1) (Leaf (2,1))
  -- Node (1,1) (Leaf (2,1)) (Node (3,2) (Leaf (5,2)) (Leaf (700,4)))

-- ----------------------------------------------
-- Lens Traversal
leaves :: Applicative f' => (a -> f' a) -> Tree a -> f' (Tree a)
leaves g (Node z l r) = Node z <$> leaves g l <*> leaves g r
leaves g (Leaf z)     = Leaf <$> (g z)

{-
  intTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

  listTree = Node [1, 1] (Leaf [2, 1]) (Node [3, 2] (Leaf [5, 2]) (Leaf [7, 4]))

  tupleTree = Node (1, 1) (Leaf (2, 1)) (Node (3, 2) (Leaf (5, 2)) (Leaf (7, 4)))
-}
main7 = do
  print $ set leaves 0 intTree
  -- Node 2 (Leaf 0) (Node 5 (Leaf 0) (Leaf 0))
  print $ over leaves (+ 1) intTree
  -- Node 2 (Leaf 4) (Node 5 (Leaf 8) (Leaf 12))
  print $ over (leaves . _1) (* 100) tupleTree
  -- Node (1,1) (Leaf (200,1)) (Node (3,2) (Leaf (500,2)) (Leaf (700,4)))
  --
  -- Compose Traversal + Traversal
  print $ over (leaves . both) (* 100) tupleTree
  -- Node (1,1) (Leaf (200,100)) (Node (3,2) (Leaf (500,200)) (Leaf (700,400)))
  --
  -- map over each elem in target container (e.g. list)
  print $ over (leaves . mapped) (* (-1)) listTree
  -- Node [1,1] (Leaf [-2,-1]) (Node [3,2] (Leaf [-5,-2]) (Leaf [-7,-4]))
  --
  -- Traversal with effects
  mapMOf_ leaves displayM tupleTree -- (2,1)
  -- (5,2)
  -- (7,4)

-- ----------------------------------------------
-- Lens.Fold
main8 = do
  print $ sumOf leaves intTree
  -- 21
  print $ anyOf leaves (> 0) intTree
  -- True
  print $ anyOf (leaves . _1) (< 0) tupleTree
  -- False
  print $ foldMapOf (leaves . _1) Sum tupleTree
  -- Sum {getSum = 14}
  print . getSum $ foldMapOf (leaves . _1) Sum tupleTree-- 14
