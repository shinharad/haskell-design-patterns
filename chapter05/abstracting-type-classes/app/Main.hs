{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- 本には書いてないが、FlexibleInstances、MultiParamTypeClassesが必要
module Main where

main :: IO ()
main = return ()

-- --------------------------------------------------------
-- Multiparameter type-classes
class Coerce a b where
  coerce :: a -> b -- coerce :: Coerce a b => a -> b

instance Coerce Int String where
  coerce = show

instance Coerce Int [Int] where
  coerce x = [x]

{-
with multiple type parameters, type inference suffers;
for example, the compiler rejects.

coerce 12 :: String

We have to help it along with type annotations:

coerce (12::Int) :: String
coerce (12::Int) :: [Int]

-}
main1 = do
  print (coerce (12 :: Int) :: String)
  print (coerce (12 :: Int) :: [Int])

-- --------------------------------------------------------
-- Functional dependencies
{-
Functional dependencies give us a way to resolve the ambiguity created by multiple type parameters.
For example, we can constrain the relationship between a and b in Coerce with a functional dependency:

The relation (b -> a) tells the compiler that if it can infer b,
it can simply look up the corresponding a in one of the type-class instances.
-}
class Coerce2 a b | b -> a where
  coerce2 :: a -> b

instance Coerce2 Int String where
  coerce2 = show

instance Coerce2 Int [Int] where
  coerce2 x = [x]

{-
bがStringの場合にaを推論させるのは、既出なのでコンパイルエラー
instance Coerce2 Float String where
   coerce2 = show
-}
main2 = do
  print (coerce2 12 :: String)
  print (coerce2 12 :: [Int])
