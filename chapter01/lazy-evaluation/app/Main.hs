module Main where

import           System.Random

main :: IO ()
main = return ()

-- Lazy Eval ------------------------------------
doomedList = [2, 3, 5, 7, undefined]

--take 0 xs     = []
--take n (x:xs) = x : (take (n - 1) xs)
main1 = print (take 4 doomedList) -- [2,3,5,7]

-- Streams --------------------------------------
infinite42s = 42 : infinite42s

potentialBoom = take 5 infinite42s

generate :: StdGen -> (Int, StdGen)
generate g = random g :: (Int, StdGen)

main2 = do
  gen0 <- getStdGen
  let (int1, gen1) = generate gen0
  let (int2, gen2) = generate gen1
  return (int1, int2)

randInts' g = (randInt, g) : randInts' nextGen
  where
    (randInt, nextGen) = generate g

randInts g = map fst (randInts' g)

randAmounts g = map (\x -> x `mod` 100) (randInts g)

main3 = do
  g <- getStdGen
  print $ take 3 (randInts g)
  print $ take 3 (randAmounts g)

-- Modeling change with streams -----------------
bankAccount openingB (amt:amts) = openingB : bankAccount (openingB + amt) amts

main4 = do
  print $ take 4 (bankAccount 100 [50, -20, 30, -10])
    -- [100,150,130,160]
  g <- getStdGen
  print $ take 4 (bankAccount 0 (randAmounts g))
