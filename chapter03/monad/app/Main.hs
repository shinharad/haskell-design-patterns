module Main where

import           Control.Applicative
import           Control.Monad

main :: IO ()
main = return ()

-- ----------------------------------------------
data Maybe' a
  = Just' a
  | Nothing'
  deriving (Show)

instance Functor Maybe' where
  fmap = liftM

instance Applicative Maybe' where
  pure = return
  (<*>) = ap

instance Monad Maybe' where
  return = Just'
  Nothing' >>= _ = Nothing'
  (Just' x) >>= f = f x

main1 = do
  print $ Just' 10 >>= \x -> Just' (show x)
    -- Just' "10"
  print $ Nothing' >>= \x -> Just' (x * 2)
    -- Nothing'

-- ----------------------------------------------
-- Levels of Functors
main2 = do
  print $ fmap (* 2) (Just' 10) -- FUNCTOR
  print $ pure (* 2) <*> Just' 10 -- APPLICATIVE
  print $ liftM (* 2) (Just' 10) -- MONAD

-- ----------------------------------------------
-- Monad as Applicative
main3 = do
  print $ (<$>) (*) (Just' 10) <*> (Just' 20) -- APPLICATIVE
  -- Just' 200
  print $ liftM2 (*) (Just' 10) (Just' 20) -- MONAD
  -- Just' 200
  print $ liftM3 f3 (Just' 10) (Just' 20) (Just' 30)
  -- Just' 6000
  where
    f3 x y z = x * y * z

-- ap_ defines <*> for Monads
ap_ mf mx = do
  f <- mf -- extract function
  x <- mx -- extract val
  return (f x)

main4 = do
  print $ (Just' (*)) `ap_` (Just' 10) `ap_` (Just' 20)

-- ----------------------------------------------
-- Sequencing actions with Monad and Applicative
action s = do
  putStrLn s
  return s

main5 = do
  let actions = map action ["the", "parts", "are", "disconnected"]
  sequence' actions
  return ()

-- Here sequence' performs the actions one after the other:
sequence' [] = return []
sequence' (x:xs) = do
  x' <- x -- action performed
  xs' <- sequence' xs
  return (x' : xs')

-- But we can also sequence actions with Applicative:
sequenceA' []     = pure []
sequenceA' (x:xs) = (:) <$> x <*> (sequenceA' xs)

main6 = do
  let actions = map action ["the", "parts", "are", "disconnected"]
  sequenceA' actions
  return ()

-- ----------------------------------------------
-- Monads and the bind chain
main7 = do
  line <- getLine -- ACTION 1
  putStrLn $ "You said " ++ line -- ACTION 2
  -- uses result of ACTION 1

main8 = mainLoop

mainLoop = do
  line <- getLine -- ACTION 1
  if line == "stop"
    then putStrLn "Bye" -- ACTION 2b
    else do
      putStrLn $ "You said " ++ line -- ACTION 2c
      mainLoop
