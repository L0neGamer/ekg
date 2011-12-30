{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Gauge.Internal
    (
      Gauge(..)
    , new
    , read
    ) where

import Data.IORef (IORef, newIORef, readIORef)
import Prelude hiding (read)

-- | A mutable, integer-valued gauge.
newtype Gauge = C { unC :: IORef Int }

-- | Create a new, zero initialized, gauge.
new :: IO Gauge
new = C `fmap` newIORef 0

read :: Gauge -> IO Int
read = readIORef . unC
