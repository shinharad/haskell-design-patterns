module Main where

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Prelude          hiding (id, (.))

import           System.IO

main :: IO ()
main = return ()

-- こんな感じで書けないものだろうかという話
-- print . length . words . readFile "jabberwocky.txt"
main1 = liftM (length . words) (readFile "jabberwocky.txt") >>= print -- 4

-- ----------------------------------------------
-- The crux of our approach will be to create a "meta type" to represent monadic IO functions and then define composition for that type:
-- IOF wraps a function (a -> IO b) and places the input and output types a and b on an equal footing, while also hiding the IO Monad.
data IOF a b =
  IOF
    { runIOF :: a -> IO b
    }

-- composition operator
(<<<<) :: IOF a b -> IOF c a -> IOF c b
(IOF f) <<<< (IOF g) = IOF $ f <=< g

-- lift a Monadic IO function into an IOF type
lift' :: (a -> b) -> IOF a b
lift' f = IOF $ return . f -- uses IO Monad's return

-- Now we can compose regular and IO functions:
main2 = do
  let f = IOF print <<<< lift' length <<<< lift' words <<<< IOF readFile
  runIOF f "jabberwocky.txt"
    -- 4
  return ()

-- ----------------------------------------------
-- Implementing an Arrow
data IOArrow a b =
  IOArrow
    { runIOArrow :: a -> IO b
    }

-- To make IOArrow a true Arrow, we need to implement Category and Arrow.
-- Category describes function composition (.).
instance Category IOArrow where
  id = IOArrow return
  -- (.) = (<<<<)
  IOArrow f . IOArrow g = IOArrow $ f <=< g

instance Arrow IOArrow where
  arr f = IOArrow $ return . f
  first (IOArrow f) =
    IOArrow $ \(a, c) -> do
      x <- f a
      return (x, c)

main3 = do
  let f = IOArrow print . arr length . arr words . IOArrow readFile
  -- vs print . length . words . readFile
  let g = IOArrow print <<< arr length <<< arr words <<< IOArrow readFile
  -- same as...
  let h = IOArrow readFile >>> arr words >>> arr length >>> IOArrow print
  runIOArrow f "jabberwocky.txt"
  runIOArrow g "jabberwocky.txt"
  runIOArrow h "jabberwocky.txt"
  -- 4

-- ----------------------------------------------
-- Arrow Operators
main4 = do
  let f =
        IOArrow readFile >>>
        arr words >>>
        arr (\x -> (x, x)) >>> -- "split" stream in 2
        first (arr length) >>> IOArrow print
  runIOArrow f "jabberwocky.txt" -- (4,["aaa","bbb","ccc","ddd"])

main5 = do
  let f =
        IOArrow readFile >>>
        arr words >>>
        arr (\x -> (x, x)) >>> -- "split" stream in 2
        first (arr length) >>> -- act on first tuple val
        second (arr (length . concat)) >>> -- act on second tuple val
        IOArrow print
  runIOArrow f "jabberwocky.txt"
    -- (4,12)

-- same as above, using *** Arrow operator
main6 = do
  let f =
        IOArrow readFile >>>
        arr words >>>
        arr (\x -> (x, x)) >>> -- "split" stream in 2
        arr length *** arr (length . concat) >>> IOArrow print
  runIOArrow f "jabberwocky.txt" -- (4,12)

-- ----------------------------------------------
-- Kleisli Arrows and Monad Arrows
main7 = do
  let f = Kleisli print . arr length . arr words . Kleisli readFile
  -- vs   IOArrow print . arr length . arr words . IOArrow readFile
  runKleisli f "jabberwocky.txt"
  -- 166
