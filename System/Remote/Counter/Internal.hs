{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Counter.Internal
    (
      Counter(..)
    , new
    , read
    ) where

import Data.Int
import Foreign.Ptr (Ptr)
import Prelude hiding (read)

-- | A mutable, integer-valued counter.
newtype Counter = C { unC :: Ptr Int64 }

-- | Create a new, zero initialized, counter.
foreign import ccall unsafe "hs_counter_new" new :: IO Counter

foreign import ccall unsafe "hs_counter_read" read :: Counter -> IO Int
