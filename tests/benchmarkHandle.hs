{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (null, length)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Criterion.Main
import Data.Monoid
import Data.Word
import Data.Iteratee
import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.IO.Fd (fileDriverFd)
import Data.Iteratee.IO.Handle (fileDriverHandle)

bufSize = 65536
file = "/usr/share/dict/words"

length' :: Monad m => Iteratee ByteString m Int
length' = length

testFdString :: IO ()
testFdString = fileDriverFd bufSize len file >> return ()
  where
  len :: Monad m => Iteratee String m Int
  len = length

testFdByte :: IO ()
testFdByte = fileDriverFd bufSize len file >> return ()
  where
  len :: Monad m => Iteratee ByteString m Int
  len = length

testHdString :: IO ()
testHdString = fileDriverHandle bufSize len file >> return ()
  where
  len :: Monad m => Iteratee String m Int
  len = length

testHdByte :: IO ()
testHdByte = fileDriverHandle bufSize len file >> return ()
  where
  len :: Monad m => Iteratee ByteString m Int
  len = length

testFdFold :: IO ()
testFdFold = fileDriverFd bufSize sum file >> return ()
 where
  sum :: Iteratee ByteString IO Word8
  sum = foldl' (+) 0

main = defaultMain
  [
   bgroup "String" [
     bench "Fd" testFdString
    ,bench "Hd with String" testHdString
   ]
  ,bgroup "ByteString" [
     bench "Fd" testFdByte
    ,bench "Hd" testHdByte
   ]
  ,bgroup "folds" [
     bench "Fd/fold" testFdFold
   ]
  ]
