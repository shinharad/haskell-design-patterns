{-# LANGUAGE Rank2Types #-}

module Main where

main :: IO ()
main = return ()

tupleF elemF (x, y) = (elemF x, elemF y)

main1 = do
  print $ tupleF length ([1, 2, 3], [3, 2, 1])
    -- :t tupleF
    -- tupleF :: (t -> b) -> (t, t) -> (b, b)
    --
    -- :t tupleF length
    -- tupleF length :: Foldable t => (t a, t a) -> (Int, Int)
    --
    -- (3,3)
  print $ tupleF show (1, 2)
    -- :t tupleF show
    -- tupleF show :: Show a => (a, a) -> (String, String)
    --
    -- ("1","2")
  print $ tupleF show (True, False)
    -- ("True","False")

tupleF' ::
     (Show a1, Show a2)
  => (forall a. Show a =>
                  a -> b)
  -> (a1, a2)
  -> (b, b)
tupleF' elemF (x, y) = (elemF x, elemF y)

main2 = do
  print $ tupleF' show (1, 2)
    -- :t tupleF' show
    -- tupleF' show :: (Show a1, Show a2) => (a1, a2) -> (String, String)
    --
    -- ("1","2")
  print $ tupleF' show (True, False)
    -- ("True","False")
  print $ tupleF' show (True, 2)
    -- ("True","2")
  print $ tupleF' show ([True, False], [1, 2])
    -- ("[True,False]","[1,2]")
