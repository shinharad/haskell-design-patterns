module Main where

import           Control.Applicative
import           Data.Maybe

main :: IO ()
main = return ()

data Maybe' a
  = Just' a
  | Nothing'
  deriving (Show)

-- we still need the Functor instance
instance Functor Maybe' where
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'
  Nothing' <*> _ = Nothing'
  _ <*> Nothing' = Nothing'
  (Just' f) <*> (Just' x) = Just' (f x)

main1 = do
  print $ pure (,) <*> Just' 2 <*> Just' 3
    -- Just' (2,3)
  print $ (,) <$> Just' 2 <*> Just' 3
    -- Just' (2,3)
  print $ Just' (.) <*> Just' (+ 2) <*> Just' (+ 3) <*> Just' 1
    -- Just' 6
  print $ Just' (+ 2) <*> (Just' (+ 3) <*> Just' 1)
    -- Just' 6
  print $ Just' (+ 3) <*> Just' 1
    -- Just' 4
