module Main where

import           Lib

main :: IO ()
main = someFunc

-- Algebraic types and pattern matching ---------
type Name = String

type Age = Int

data Person =
  P String Int -- combination

data MaybeInt
  = NoInt
  | JustInt Int

maybeInts = [JustInt 2, JustInt 3, JustInt 5, NoInt]

data Maybe' a
  = Nothing'
  | Just' a
  deriving (Show)

fMaybe f (Just' x) = Just' (f x)
fMaybe f Nothing'  = Nothing'

fMaybes = map (fMaybe (* 2)) [Just' 2, Just' 3, Nothing']
  -- [Just' 4,Just' 6,Nothing']

-- Recursive types ------------------------------
data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

size :: Tree a -> Int
size (Leaf x)     = 1
size (Branch t u) = size t + size u + 1

main1 = print $ size tree
  where
    tree = Branch (Leaf 1) (Branch (Leaf 2) (Leaf 7))

-- Parametric polymorphism ----------------------
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length xs

main2 = do
  print $ length' [1, 2, 3, 5, 7]
  -- 5
  print $ length' ['1', '2', '3', '5', '7']
  -- 5

-- Ad-hoc polymorphism --------------------------
{-
class Num a where
   (+) :: a -> a -> a

instance Int Num where
   (+) :: Int → Int → Int
   x + y = intPlus x y

instance Float Num where
   (+) :: Float → Float → Float
   x + y = floatPlus x y
-}
-- Alternation based ad-hoc polymorphism -------
data Shape
  = Circle Float
  | Rect Float Float

area :: Shape -> Float
area (Circle r)          = pi * r ^ 2
area (Rect length width) = length * width

main3 = do
  print $ area (Circle 1)
  -- 3.1415927
  print $ area (Rect 2 3)
  -- 6.0

-- Class-based ad-hoc polymorphism --------------
data Circle' =
  Circle' Float

data Rect' =
  Rect' Float Float

class Shape' a where
  area' :: a -> Float

instance Shape' Circle' where
  area' (Circle' r) = pi * r ^ 2

instance Shape' Rect' where
  area' (Rect' length' width') = length' * width'

main4 = do
  print $ area' (Circle' 1)
  -- 3.1415927
  print $ area' (Rect' 2 3)
  -- 6.0

-- Polymorphic dispatch and the visitor pattern -
data CustomerEvent
  = InvoicePaid Float
  | InvoiceNonPayment

data Customer
  = Individual Int
  | Organisation Int

payment_handler :: CustomerEvent -> Customer -> String
payment_handler (InvoicePaid amt) (Individual custId) = "SendReceipt for " ++ (show amt)
payment_handler (InvoicePaid amt) (Organisation custId) = "SendReceipt for " ++ (show amt)
payment_handler InvoiceNonPayment (Individual custId) = "CancelService for " ++ (show custId)
payment_handler InvoiceNonPayment (Organisation custId) = "SendWarning for " ++ (show custId)

main5 = do
  print $ payment_handler (InvoicePaid 112.2) (Individual 1)
  print $ payment_handler (InvoicePaid 112.2) (Organisation 11)
  -- "SendReceipt for 112.2"
  print $ payment_handler InvoiceNonPayment (Individual 1)
  -- "CancelService for 1"
  print $ payment_handler InvoiceNonPayment (Organisation 11)
  -- "SendWarning for 11"
