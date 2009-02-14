{-# LANGUAGE CPP #-}

module Data.Iteratee.IO.Base (
#if defined(USE_WINDOWS)
  module Data.Iterate.IO.Windows
#endif
#if defined(USE_POSIX)
  module Data.Iteratee.IO.Posix
#endif
)
where

#if defined(USE_WINDOWS)
import Data.Iteratee.IO.Windows
#endif

#if defined(USE_POSIX)
import Data.Iteratee.IO.Posix
#endif

