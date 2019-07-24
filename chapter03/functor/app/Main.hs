module Main where

main :: IO ()
main = return ()

f :: Num a => a -> a
f = (^ 2)

main1 = do
  print $ f 5
  print $ f 5.0

-- ----------------------------------------------
data Maybe' a
  = Just' a
  | Nothing'
  deriving (Show)

instance Functor Maybe' where
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' (f x)

main2 = do
  print $ fmap f (Just' 42)
    -- Just' 1764
  print $ fmap show (Just' 42)
    -- Just' "42"

-- ----------------------------------------------
{-
-- law of composition
fmap (f . g)  ==  fmap f . fmap g

-- e.g.
fmap (f . read) getLine  -- is the same as
(fmap f) . (fmap read) $ getLine

-- identity law
fmap id  ==  id
-- e.g.
fmap id (Just 1) = id (Just 1)
-}
-- ----------------------------------------------
main3 = do
  print $ map (^ 2) [1, 2, 3, 5, 7]
  print $ fmap (^ 2) [1, 2, 3, 5, 7]
