module Main where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (chr)
import           System.IO

main :: IO ()
main = return ()

main1 = do
  h <- openFile "jabberwocky.txt" ReadMode
  loop h
  hClose h
  where
    loop h' = do
      isEof <- hIsEOF h'
      if isEof
        then putStrLn "DONE..."
        else do
          line <- hGetLine h'
          print $ words line
          loop h'
          -- ["a","b","c","d"]
          -- ["e","f","g","h"]

-- Instead of the hGetLine function, let's use a Data.ByteString.hGet function to read from the file in chunks of 8 bytes:
main2 = do
  h <- openFile "jabberwocky.txt" ReadMode
  loop h
  hClose h
  where
    loop h' = do
      isEof <- hIsEOF h'
      if isEof
        then putStrLn "DONE..."
        else do
          chunk <- B.hGet h' 8
          print . words . show $ chunk
          loop h' -- ["\"a","b","c","d\\n\""]
          -- ["\"e","f","g","h\""]

data Chunk
  = Chunk
      { chunk :: String
      }
  | LineEnd
      { chunk     :: String
      , remainder :: String
      }
  deriving (Show)

-- toS = map (chr . fromEnum) . B.unpack
--   B.unpack :: B8.ByteString -> [GHC.Word.Word8]
--   (chr . fromEnum) :: Enum a => a -> Char
parseChunk chunk =
  if rightS == B8.pack ""
    then Chunk (toS leftS)
    else LineEnd (toS leftS) ((toS . B8.tail) rightS)
  where
    (leftS, rightS) = B8.break (== '\n') chunk
    toS = map (chr . fromEnum) . B.unpack

main3 = do
  print $ parseChunk (B8.pack "AAA\nBB")
  -- LineEnd {chunk = "AAA", remainder = "BB"}
  print $ parseChunk (B8.pack "CCC")
  -- Chunk {chunk = "CCC"}

main4 = do
  fileH <- openFile "jabberwocky.txt" ReadMode
  loop "" fileH
  hClose fileH
  where
    loop acc h = do
      isEof <- hIsEOF h
      if isEof
        then do
          putStrLn acc
          putStrLn "DONE..."
        else do
          chunk <- B.hGet h 8
          case parseChunk chunk of
            Chunk chunk' -> do
              let accLine = acc ++ chunk'
              loop accLine h
            LineEnd chunk' remainder -> do
              let line = acc ++ chunk'
              putStrLn line -- do something with line
              loop remainder h
          return ()
