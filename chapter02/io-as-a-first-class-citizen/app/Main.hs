module Main where

import           Control.Applicative
import           Control.Monad
import           System.IO

main :: IO ()
main = return ()

-- I/O as a first class citizen -----------------
main1 = do
  h <- openFile "jabberwocky.txt" ReadMode
  line <- hGetLine h
  putStrLn . show . words $ line
  -- ["a","b","c","d"]
  hClose h

main2 = do
  h <- openFile "jabberwocky.txt" ReadMode
  hGetLine h >>= print . words
  -- ["a","b","c","d"]
  hClose h

-- I/O as a functor, applicative, and monad -----
main3 = do
  h <- openFile "jabberwocky.txt" ReadMode
  line <- fmap (show . words) (hGetLine h)
  putStrLn line
  -- ["a","b","c","d"]
  hClose h

main4 = do
  h <- openFile "jabberwocky.txt" ReadMode
  line <- show . words <$> hGetLine h
  putStrLn line
  -- ["a","b","c","d"]
  hClose h

main5 = do
  h <- openFile "jabberwocky.txt" ReadMode
  line <- liftM (show . words) (hGetLine h)
  putStrLn line
  -- ["a","b","c","d"]
  hClose h
