module Main where

import           Control.Monad
import           System.IO

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import           Data.Char                  (chr)

import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = return ()

-- lazy IO stream
main1 = do
  let ios = map putStrLn ["this", "won't", "run"]
  putStrLn "until ios is 'sequenced'..."
  sequence_ ios -- perform actions
    -- until ios is 'sequenced'...
    -- this
    -- won't
    -- run

-- sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
-- sequence_ = foldr (>>) (return ())
main2 = do
  h <- openFile "jabberwocky.txt" ReadMode
  line1 <- hGetLine h -- perform action
  let getLines = [hGetLine h, hGetLine h]
  [line2, line3] <- sequence getLines -- perform actions
  hClose h
  putStrLn line1 -- a b c d
  putStrLn line2 -- "e f g h"
  putStrLn line3 -- "i j k l"

main3 = do
  h <- openFile "jabberwocky.txt" ReadMode
  contents <- hGetContents h -- (lazy) stream
  putStrLn (take 10 contents) -- lazily fetch 10 chars
  hClose h
    -- a b c d
    -- e

-- ----------------------------------------------
lineStream h' = lines <$> hGetContents h' -- fmapで良い
  -- lineStream h' = hGetContents h' >>= return . lines

-- Using lines together with hGetContents, we get a lazy stream of file lines:
main4 = do
  h <- openFile "jabberwocky.txt" ReadMode
  lines' <- lineStream h
  sequence_ (map putStrLn lines')
  hClose h

-- The mapM function captures the common pattern of mapping and sequencing:
main5 = do
  h <- openFile "jabberwocky.txt" ReadMode
  lines' <- lineStream h
  mapM_ putStrLn lines'
  hClose h

-- "trailing lambda"
main6 = do
  h <- openFile "jabberwocky.txt" ReadMode
  lines' <- lineStream h
  forM_ lines' $ \line -> do
    let reversed = reverse line
    putStrLn reversed
  hClose h

data Chunk
  = Chunk
      { chunk :: String
      }
  | LineEnd
      { chunk     :: String
      , remainder :: String
      }
  deriving (Show)

parseChunk chunk =
  if rightS == B8.pack ""
    then Chunk (toS leftS)
    else LineEnd (toS leftS) ((toS . B8.tail) rightS)
  where
    (leftS, rightS) = B8.break (== '\n') chunk
    toS = map (chr . fromEnum) . B.unpack

main7 = print $ parseChunk (B8.pack "gimble in the wabe:\nAll")
  -- LineEnd {chunk = "gimble in the wabe:", remainder = "All"}

-- ----------------------------------------------
chunkStream :: Handle -> IO [L8.ByteString]
chunkStream h = do
  isEof <- hIsEOF h
  if isEof
    then return []
    else do
      chunk <- LB.hGet h 8
      rest <- chunkStream h
      return (chunk : rest)

main8 = do
  h <- openFile "jabberwocky.txt" ReadMode
  chunks <- chunkStream h
  print $ take 10 chunks
    -- ["a b c d\n","e f g h\n","i j k l"]
-- ----------------------------------------------
-- The chunk stream produces data. Next, we write a consumer:
