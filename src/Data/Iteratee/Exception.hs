{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

-- |Monadic and General Iteratees:
-- Messaging and exception handling.
--
-- Iteratees use an internal exception handling mechanism that is parallel to
-- that provided by 'Control.Exception'.  This allows the iteratee framework
-- to handle its own exceptions outside @IO@.
--
-- Enumerators can be constructed to handle a 'SomeException' with
-- @Data.Iteratee.Iteratee.enumFromCallbackCatch@.  If the enumerator detects
-- an @iteratee exception@, the enumerator calls the provided exception handler.
-- The enumerator is then able to continue feeding data to the iteratee,
-- provided the exception was successfully handled.  If the handler could
-- not handle the exception, the exception is converted to an
-- 'EnumException' and processing aborts.
--
-- Exceptions can also be cleared by @Data.Iteratee.Iteratee.checkErr@,
-- although in this case the iteratee continuation cannot be recovered.
--
-- When viewed as Resumable Exceptions, iteratee exceptions provide a means
-- for iteratees to send control messages to enumerators.  The @seek@
-- implementation provides an example.  @Data.Iteratee.Iteratee.seek@ stores
-- the current iteratee continuation and throws a 'SeekException', which
-- inherits from 'IterException'.  @Data.Iteratee.IO.enumHandleRandom@ is
-- constructed with @enumFromCallbackCatch@ and a handler that performs
-- an @hSeek@.  Upon receiving the 'SeekException', @enumHandleRandom@ calls
-- the handler, checks that it executed properly, and then continues with
-- the stored continuation.
--
-- As the exception hierarchy is open, users can extend it with custom
-- exceptions and exception handlers to implement sophisticated messaging
-- systems based upon resumable exceptions.


module Data.Iteratee.Exception (
  -- ** Enumerator exceptions
  EnumException (..)
  ,DivergentException (..)
  -- ** Iteratee exceptions
  ,SeekException (..)
  ,EofException (..)
)
where

import Data.Iteratee.IO.Base

import Control.Exception
import Data.Data


-- Root of enumerator exceptions.
data EnumException = forall e . Exception e => EnumException e
  deriving Typeable

instance Show EnumException where
  show (EnumException e) = show e

instance Exception EnumException

-- |The @iteratee@ diverged upon receiving 'EOF'.
data DivergentException = DivergentException
  deriving (Show, Typeable)

instance Exception DivergentException

-- |A seek request within an @Iteratee@.
data SeekException = SeekException FileOffset
  deriving (Typeable, Show)

instance Exception SeekException

-- |The @Iteratee@ needs more data but received @EOF@.
data EofException = EofException
  deriving (Typeable, Show)

instance Exception EofException