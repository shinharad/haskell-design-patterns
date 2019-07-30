module Main where

main :: IO ()
main = return ()

data Expr1
  = I1 Int
  | Add1 Expr1 Expr1

eval1 :: Expr1 -> Int
eval1 (I1 v)     = v
eval1 (Add1 x y) = eval1 x + eval1 y

main1 = do
  print $ eval1 (I1 7)
    -- 7
  print $ eval1 (Add1 (I1 7) (I1 5))
    -- 12

-- --------------------------------------------------------
data Expr2
  = I2 Int
  | B2 Bool
  | Add2 Expr2 Expr2
  deriving (Show)

eval2 :: Expr2 -> Int -- Int or Boolでなければならない
eval2 (I2 v)     = v
--eval2 (B2 v)     = v -- INVALID
eval2 (Add2 x y) = eval2 x + eval2 y

-- INVALID
--eval2 :: Expr2 -> t
--eval2 (I2 v) = v
--eval2 (B2 v) = v
--eval2 (Add2 x y) = eval2 x + eval2 y
main2
  --  print $ eval2 (Add2 (I2 7) (B2 True)) -- Exception!!!
 = do
  print $ eval2 (Add2 (I2 7) (I2 5))

-- --------------------------------------------------------
-- Phantom types solve our first problem by adding the type parameter t to the Expr3 type:
data Expr3 t
  = I3 Int
  | B3 Bool
  | Add3 (Expr3 Int) (Expr3 Int)
  deriving (Show)

-- The type t serves as a type placeholder that can be used by each constructor to describe its particular type.
-- However, all the constructors still return the same type:
{-
  I3   ::                    Int -> Expr3 t
  B3   ::                   Bool -> Expr3 t
  Add3 :: Expr3 Int -> Expr3 Int -> Expr3 t
-}
-- We can still construct invalid values:
main3 = do
  print $ Add3 (I3 11) (B3 True)

-- --------------------------------------------------------
-- we can use the phantom type information to create type-safe smart constructors:
i3 :: Int -> Expr3 Int
i3 = I3

b3 :: Bool -> Expr3 Bool
b3 = B3

add3 :: Expr3 Int -> Expr3 Int -> Expr3 Int
add3 = Add3

-- If we use the smart constructors instead of data-type constructors,
-- the Haskell type-checker will prevent us from creating
-- If we use smart constructors instead of data-type constructors,
-- the type-checker can reject "bad" expressions
main4 = do
  print $ Add3 (I3 7) (B3 True)
  -- Add3 (I3 7) (B3 True)
  print $ add3 (i3 10) (i3 7) -- Add3 (I3 10) (I3 7)
  -- rejected by compiler
  --  print $ add3 (i3 10) (b3 True) -- INVALID
-- --------------------------------------------------------
{-
-- However, type inference remains a problem because the values are still not described accurately. For example:
(I3 12) :: Expr3 t   -- this
--      :: Expr3 Int – not this

-- The effect is that adding values remains too ambiguous:
eval3 :: Expr3 -> t
eval3 (Add3 x y) = (eval3 x) + (eval3 y)
-}
