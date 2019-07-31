{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

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
  print $ showT (RList RChar) ['2', '3', '5'] -- "'2' :: Char, '3' :: Char, '5' :: Char, THE END"

-- --------------------------------------------------------
-- DYNAMIC TYPES - using existential quantification:
-- Even though DynEq dynamic values have opaque type, they are well typed.
data DynamicEQ =
  forall t. Show t =>
            DynEQ (Rep t) t

-- heterogeneous list using a Dynamic type
-- Even though DynEq dynamic values have opaque type, they are well typed.
-- For example, we can use them to express heterogeneous lists:
dynEQList :: [DynamicEQ]
dynEQList = [DynEQ RChar 'x', DynEQ RInt 3]

-- --------------------------------------------------------
-- DYNAMIC TYPES - using GADTs
-- Since GADTs generalize existentials,
-- we can also write the above example in the form of a "dynamic GADT":
data Dynamic where
  Dyn :: Show t => Rep' t -> t -> Dynamic

instance Show Dynamic where
  show (Dyn rep v) = showT2 rep v

-- heterogeneous list using GADTs
dynList :: [Dynamic]
dynList = [Dyn RChar' 'x', Dyn RInt' 3]

showDyn (Dyn rep v) = showT2 rep v

-- redefine
data Rep' t where
  RInt' :: Rep' Int
  RChar' :: Rep' Char
  RList' :: Show a => Rep' a -> Rep' [a]
  RDyn :: Rep' Dynamic -- Add

showT2 :: Show t => Rep' t -> t -> String
showT2 RInt' i             = show i ++ " :: INT"
showT2 RChar' i            = show i ++ " :: Char"
showT2 (RList' rep) []     = "THE END"
showT2 (RList' rep) (x:xs) = showT2 rep x ++ ", " ++ showT2 (RList' rep) xs
showT2 RDyn (Dyn rep v)    = showT2 rep v -- NEW clause for RDyn constructor

main3 = do
  print $ showT2 RInt' 17
    -- "17 :: INT"
  print $ showT2 (RList' RInt') [12, 13, 14]
    -- "12 :: INT, 13 :: INT, 14 :: INT, THE END"
  print $ showT2 (RList' RDyn) dynList
    -- "'x' :: Char, 3 :: INT, THE END"

main4 = mapM_ (putStrLn . showDyn) dynList
          -- 'x' :: Char
          -- 3 :: INT

-- Dynamic types carry enough type information about themselves to enable safe type casting:
toInt :: Dynamic -> Maybe Int
toInt (Dyn RInt' i) = Just i
toInt (Dyn _ _)     = Nothing

-- --------------------------------------------------------
-- HETEROGENEOUS LISTS -- Using existentials
-- --------------------------
-- Existential Type v1
-- --------------------------
data LI_Eq1 =
  forall a. LI_Eq1 a

hListEq1 :: [LI_Eq1]
hListEq1 = [LI_Eq1 3, LI_Eq1 "5"]

-- --------------------------
-- Existential Type v2
-- --------------------------
-- For example, in order to show list items we need to package a show function with each item:
data LI_Eq2 =
  forall a. LI_Eq2 a (a -> String)

hListEq2 :: [LI_Eq2]
hListEq2 = [LI_Eq2 3 (show :: Int -> String), LI_Eq2 "5" (show :: String -> String)]

-- (We add the show type signatures here for the sake of clarity but they can be inferred, in other words omitted.)
showEq2 (LI_Eq2 v showF) = showF v

-- e.g. main = mapM_ (putStrLn . showEq2) hListEq2
-- --------------------------
-- Existential Type v3
-- --------------------------
-- Instead of passing in the show functions explicitly,
-- we can constrain the type signatures to the Show type­class,
-- allowing us to implicitly supply the show functions:
--
-- The type-class constraint specified in the existential amounts to what is called bounded quantification (bounded by type-class).
data LI_Eq3 =
  forall a. Show a =>
            LI_Eq3 a

hListEq3 :: [LI_Eq3]
hListEq3 = [LI_Eq3 3, LI_Eq3 "5"]

showEq3 (LI_Eq3 v) = show v

main5 = mapM_ (putStrLn . showEq3) hListEq3

-- --------------------------------------------------------
-- HETEROGENEOUS LISTS -- Using GADTs
-- --------------------------
-- GADT v1
-- --------------------------
data LI_Gadt1 where
  MkShow1 :: a -> (a -> String) -> LI_Gadt1

hListGadt1 :: [LI_Gadt1]
hListGadt1 = [MkShow1 "3" show, MkShow1 5 show]

showGadt1 (MkShow1 v showF) = showF v

main6 = mapM_ (putStrLn . showGadt1) hListGadt1

-- --------------------------
-- GADT v2
-- --------------------------
-- bounded quantification
data LI_Gadt2 where
  MkShow2 :: Show a => a -> LI_Gadt2

hListGadt2 :: [LI_Gadt2]
hListGadt2 = [MkShow2 "3", MkShow2 5]

showGadt2 (MkShow2 v) = show v

main7 = mapM_ (putStrLn . showGadt2) hListGadt2
