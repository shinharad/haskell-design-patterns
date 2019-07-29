{-# LANGUAGE ExistentialQuantification #-}

module Main where

main :: IO ()
main = return ()

-- ------------------------------------------------------
-- As another example, consider the universally quantified type ObjU,
-- which mimics an OOP object with one property (of type a) and two object methods:
data ObjU a =
  ObjU
    a -- property
    (a -> Bool) -- obj method
    (a -> String) -- obj method

obj_f1 :: ObjU a -> Bool
obj_f1 (ObjU v f1 _) = f1 v

obj_f2 :: ObjU a -> String
obj_f2 (ObjU v _ f2) = f2 v

main1 = do
  print $ obj_f1 obj
  -- False
  print $ obj_f2 obj
  -- "3"
  where
    obj = ObjU 3 even show

-- ------------------------------------------------------
-- Existential quantification and abstract datatypes
data ObjE =
  forall a. ObjE a (a -> Bool) (a -> String)

objE_f1 :: ObjE -> Bool
objE_f1 (ObjE v f1 _) = f1 v

objE_f2 :: ObjE -> String
objE_f2 (ObjE v _ f2) = f2 v

main2 = do
  print $ objE_f1 obj
  -- False
  print $ objE_f2 obj
  -- "3"
  where
    obj = ObjE 3 even show
