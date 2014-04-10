{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Counter.Internal
    (
      Counter(..)
    , new
    , read
    ) where

import Data.Int
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Prelude hiding (read)

-- | A mutable, integer-valued counter.
newtype Counter = C { unC :: ForeignPtr Int }

-- | Create a new, zero initialized, counter.
new :: IO Counter
new = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ p -> poke p 0
    return $ C fp

read :: Counter -> IO Int
read (C fp) = withForeignPtr fp cRead

foreign import ccall unsafe "hs_counter_read" cRead :: Ptr Int -> IO Int
