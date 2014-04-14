{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Gauge.Internal
    (
      Gauge(..)
    , new
    , read
    ) where

import Prelude hiding (read)

import qualified Data.Atomic as Atomic

-- | A mutable, integer-valued gauge.
newtype Gauge = C { unC :: Atomic.Atomic }

-- | Create a new, zero initialized, gauge.
new :: IO Gauge
new = C `fmap` Atomic.new 0

read :: Gauge -> IO Int
read = Atomic.read . unC
