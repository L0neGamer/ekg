{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Counter.Internal
    (
      Counter(..)
    , new
    , read
    ) where

import Prelude hiding (read)

import qualified Data.Atomic as Atomic

-- | A mutable, integer-valued counter.
newtype Counter = C { unC :: Atomic.Atomic }

-- | Create a new, zero initialized, counter.
new :: IO Counter
new = C `fmap` Atomic.new 0

read :: Counter -> IO Int
read = Atomic.read . unC
