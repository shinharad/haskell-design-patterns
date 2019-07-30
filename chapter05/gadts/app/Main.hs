{-# LANGUAGE GADTs #-}

module Main where

main :: IO ()
main = return ()

-- --------------------------------------------------------
-- a GADT with phantom type 't' and built-in smart constructors
data Expr t where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int

eval :: Expr t -> t
eval (I v)     = v
eval (B v)     = v
eval (Add x y) = eval x + eval y

main1 = do
  print $ eval (Add (I 10) (I 12))
  -- print $ eval (Add (I 10) (B True)) INVALID

-- --------------------------------------------------------
{-

THE TYPECASE PATTERN

"TypeCase: a design pattern that allows the definition of closed type-indexed functions,
in which the index family is fixed but the collection of functions is extensible"

-}
-- The RList constructor can be thought of as being existentially qualified (a does not appear on the left-hand side).
-- The phantom t in Rep t will serve as type metadata.
data Rep t where
  RInt :: Rep Int
  RChar :: Rep Char
  RList :: Show a => Rep a -> Rep [a]

-- We can now write a function that takes a value along with its type representation:
-- showT is a closed type-indexed function because the type index family (Rep t) is fixed.
showT :: Show t => Rep t -> t -> String
showT RInt i             = show i ++ " :: INT"
showT RChar i            = show i ++ " :: Char"
showT (RList rep) []     = "THE END"
showT (RList rep) (x:xs) = showT rep x ++ ", " ++ showT (RList rep) xs

-- The showT function is a type-indexed function because it is defined for each member of the family of types Rep t:
main2 = do
  print $ showT RInt 3
  -- "3 :: INT"
  print $ showT (RList RInt) [12, 13, 14]
  -- "12 :: INT, 13 :: INT, 14 :: INT, THE END"
  print $ showT (RList RChar) ['2', '3', '5']
  -- "'2' :: Char, '3' :: Char, '5' :: Char, THE END"

-- --------------------------------------------------------

