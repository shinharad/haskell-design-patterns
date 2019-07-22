module Main where

main :: IO ()
main = return ()

-- Functions as first-class citizens ------------
square = \x -> x * x

main1 = print $ map square [1, 3, 5, 7]
  -- [1,9,25,49]

main2 = print $ sum [1, 3, 5, 7]
  where
    sum = foldr (+) 0
  -- 16

main3 = print $ zipWith (\f v -> f v) fs [1, 3, 5]
  where
    fs = [(* 2), (* 3), (* 5)]
  -- [2,9,25]

-- Composing functions --------------------------
f, g, h :: String -> String
f x = "(" ++ x ++ ")"

g x = "<" ++ x ++ ">"

h x = "{" ++ x ++ "}"

main4 = do
  print $ z "x"
  -- "(<{x}>)"
  print $ z' "x"
  -- "(<{x}>)"
  print $ z'' "x"
  -- "(<{x}>)"
  where
    z x = f (g (h x))
    z' x = (f . g . h) x
    z'' = f . g . h

-- Currying functions ---------------------------
greetCurried :: String -> String -> String
greetCurried title name = "Greetings " ++ title ++ " " ++ name

greetUncurried :: (String, String) -> String
greetUncurried (title, name) = "Greetings " ++ title ++ " " ++ name

greetCurried' :: String -> String
greetCurried' = greetCurried "Ms"

greetUncurried' :: String -> String
greetUncurried' name = greetUncurried ("Ms", name)

main5 = do
  print $ greetCurried "abc" "xyz"
  -- "Greetings abc xyz"
  print $ greetUncurried ("abc", "xyz")
  -- "Greetings abc xyz"
  print $ greetCurried' "abc"
  -- "Greetings Ms abc"
  print $ greetUncurried' "abc"
  -- "Greetings Ms abc"

-- Currying and composability -------------------
g' n = (n ^ 2, n ^ 3)

main6 = do
  print $ uncurry max (g' 11)
  -- 1331
  print $ map (map square) [[1], [2, 2], [3, 3, 3]]
  -- [[1],[4,4],[9,9,9]]
  print $ (curry map') (curry map' square) [[1], [2, 2], [3, 3, 3]]
  -- [[1],[4,4],[9,9,9]]
  where
    map' = uncurry map

-- Non-tail / Tail recursion ---------------------------
sumNonTail [] = 0
sumNonTail (x:xs) = x + (sumNonTail xs)

sumTail' acc [] = acc
sumTail' acc (x:xs) = sumTail' (acc + x) xs
sumTail xs = sumTail' 0 xs

main7 = do
  print $ sumNonTail [2, 3, 5, 7]
  -- 17
  print $ sumTail [2, 3, 5, 7]
  -- 17

-- Folding abstracts recursion ------------------

foldlSum = foldl (+) 0
foldrSum = foldr (+) 0

--foldr _ v [] = v
--foldr f v (x:xs) = f x (foldr f v xs)

--foldl _ v [] = v
--foldl f v (x:xs) = foldl f (f v x) xs

main8 = do
  print $ foldlSum [2, 3, 5, 7]
  -- 17
  print $ foldrSum [2, 3, 5, 7]
  -- 17





