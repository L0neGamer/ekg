{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
-- | An atomic integer value. All operations are thread safe.
module Data.Atomic
    (
      Atomic
    , new
    , read
    , inc
    , add
    ) where

import Data.Int
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Prelude hiding (read)

-- | A mutable, atomic integer.
newtype Atomic = C (ForeignPtr Int)

-- | Create a new, zero initialized, atomic.
new :: Int -> IO Atomic
new n = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ p -> poke p n
    return $ C fp

read :: Atomic -> IO Int
read (C fp) = withForeignPtr fp cRead

foreign import ccall unsafe "hs_atomic_read" cRead :: Ptr Int -> IO Int

-- | Increase the atomic by one.
inc :: Atomic -> IO ()
inc atomic = add atomic 1

add :: Atomic -> Int -> IO ()
add (C fp) n = withForeignPtr fp $ \ p -> cAdd p n

-- | Increase the atomic by the given amount.
foreign import ccall unsafe "hs_atomic_add" cAdd :: Ptr Int -> Int -> IO ()
