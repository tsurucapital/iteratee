{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Iteratee.IO.Handle(
  -- * File enumerators
  enumHandle
  ,enumHandleCatch
  ,enumHandleRandom
  ,enumFile
  ,enumFileRandom
  -- * Iteratee drivers
  ,fileDriverHandle
  ,fileDriverRandomHandle
)

where
import Data.ByteString (ByteString, packCStringLen)
import Data.Word (Word8)
import Data.Iteratee.Iteratee

import Control.Exception
import Control.Monad
import Control.Monad.Catch as CIO
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO

-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makeHandleCallback ::
  (MonadIO m) =>
  Ptr Word8
  -> Int
  -> Handle
  -> st
  -> m (Either SomeException ((Bool, st), ByteString))
makeHandleCallback p bsize h st = do
  n' <- liftIO (CIO.try $ hGetBuf h p bsize :: IO (Either SomeException Int))
  case n' of
    Left e -> return $ Left e
    Right 0 -> return $ Right ((False, st), empty)
    Right n -> liftM (\s -> Right ((True, st), s)) $
                liftIO $ packCStringLen (castPtr p, fromIntegral n)


-- |The (monadic) enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
-- Data is read into a buffer of the specified size.
enumHandle ::
 forall m a.(MonadIO m, MonadMask m) =>
  Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> Enumerator ByteString m a
enumHandle bufsize h i =
  CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallback (makeHandleCallback p bufsize h) () i)

-- |An enumerator of a file handle that catches exceptions raised by
-- the Iteratee.
enumHandleCatch
 ::
 forall e m a.(Exception e,
                    MonadIO m, MonadMask m) =>
  Int -- ^Buffer size (number of bytes per read)
  -> Handle
  -> (e -> m (Maybe EnumException))
  -> Enumerator ByteString m a
enumHandleCatch bufsize h handler i =
  CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallbackCatch (makeHandleCallback p bufsize h) handler () i)


-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests).
-- Data is read into a buffer of the specified size.
enumHandleRandom ::
 forall m a.(MonadIO m, MonadMask m) =>
  Int -- ^ Buffer size (number of elements per read)
  -> Handle
  -> Enumerator ByteString m a
enumHandleRandom bs h i = enumHandleCatch bs h handler i
  where
    handler (SeekException off) =
       liftM (either
              (Just . EnumException :: IOException -> Maybe EnumException)
              (const Nothing))
             . liftIO . CIO.try $ hSeek h AbsoluteSeek $ fromIntegral off

-- ----------------------------------------------
-- File Driver wrapper functions.

enumFile' :: (MonadIO m, MonadMask m) =>
  (Int -> Handle -> Enumerator ByteString m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator ByteString m a
enumFile' enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openBinaryFile filepath ReadMode)
  (liftIO . hClose)
  (flip (enumf bufsize) iter)

enumFile ::
  (MonadIO m, MonadMask m)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator ByteString m a
enumFile = enumFile' enumHandle

enumFileRandom ::
  (MonadIO m, MonadMask m)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator ByteString m a
enumFileRandom = enumFile' enumHandleRandom

-- |Process a file using the given @Iteratee@.  This function wraps
-- @enumHandle@ as a convenience.
fileDriverHandle
  :: (MonadIO m, MonadMask m) =>
     Int                      -- ^Buffer size (number of elements)
     -> Iteratee ByteString m a
     -> FilePath
     -> m a
fileDriverHandle bufsize iter filepath =
  enumFile bufsize filepath iter >>= run

-- |A version of @fileDriverHandle@ that supports seeking.
fileDriverRandomHandle
  :: (MonadIO m, MonadMask m) =>
     Int                      -- ^ Buffer size (number of elements)
     -> Iteratee ByteString m a
     -> FilePath
     -> m a
fileDriverRandomHandle bufsize iter filepath =
  enumFileRandom bufsize filepath iter >>= run

