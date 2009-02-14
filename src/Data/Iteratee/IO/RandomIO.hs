{-# LANGUAGE FlexibleContexts #-}

-- Random and Binary IO with IterateeM

module Data.Iteratee.IO.RandomIO (
  bindm,
  stakeR,
  enum_fd_random,
  file_driver_rb
)

where

import Data.Iteratee.Base
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base
import Data.Word
import Data.Int

import System.Posix hiding (FileOffset)
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO (SeekMode(..))


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- The enumerator of a POSIX Fd: a variation of enum_fd that
-- supports RandomIO (seek requests)
enum_fd_random :: ReadableChunk s Word8 => Fd -> EnumeratorGM s Word8 IO a
enum_fd_random fd iter = {-# SCC "enum_fd_random" #-}
    IM $ allocaBytes (fromIntegral buffer_size) (loop (0,0) iter)
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = 4096
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
  loop :: (ReadableChunk s Word8) =>
          (FileOffset,Int) ->
          IterateeG s Word8 IO a -> 
	  Ptr Word8 ->
          IO (IterateeG s Word8 IO a)
  loop _pos iter'@IE_done{} _p = return iter'
  loop pos@(off,len) (IE_jmp off' c) p | 
    off <= off' && off' < off + fromIntegral len =	-- Seek within buffer p
    do
    let local_off = fromIntegral $ off' - off
    s <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    im <- unIM $ c (Chunk s)
    loop pos im p
  loop _pos iter'@(IE_jmp off c) p = do -- Seek outside the buffer
   off' <- myfdSeek fd AbsoluteSeek (fromIntegral off)
   case off' of
    Left _errno -> unIM $ enum_err "IO error" iter'
    Right off''  -> loop (off'',0) (IE_cont c) p
    -- Thanks to John Lato for the strictness annotation
    -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop (off,len) iter'@(IE_cont step) p = do
   n <- myfdRead fd (castPtr p) buffer_size
   case n of
    Left _errno -> unIM $ step (Err "IO error")
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         im <- unIM $ step (Chunk s)
	 loop (off + fromIntegral len,fromIntegral n') im p

-- |Process a file using the given IterateeGM
file_driver_rb :: ReadableChunk s Word8 => IterateeGM s Word8 IO a ->
               FilePath ->
               IO (Either (String, a) a)
file_driver_rb iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- unIM $ (enum_fd_random fd >. enum_eof) ==<< iter
  closeFd fd
  print_res result
 where
  print_res (IE_done a (Err err)) = return $ Left (err, a)
  print_res (IE_done a _) = return $ Right a
  print_res (IE_cont _) = return $ Left ("Iteratee unfinished", undefined)
  print_res (IE_jmp _ _) = return $ Left ("Iteratee unfinished", undefined)
                           
