{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, integer-valued gauges.
-- Gauges are variable values and can be used to track e.g. the
-- current number of concurrent connections. All operations on gauges
-- are thread-safe.
module System.Remote.Gauge
    (
      Gauge
    , inc
    , dec
    , add
    , subtract
    , set
    ) where

import Prelude hiding (subtract)

import qualified Data.Atomic as Atomic
import System.Remote.Gauge.Internal

-- | Increase the gauge by one.
inc :: Gauge -> IO ()
inc gauge = add gauge 1

-- | Decrease the gauge by one.
dec :: Gauge -> IO ()
dec gauge = subtract gauge 1

-- | Increase the gauge by the given amount.
add :: Gauge -> Int -> IO ()
add gauge = Atomic.add (unC gauge)

-- | Decrease the gauge by the given amount.
subtract :: Gauge -> Int -> IO ()
subtract gauge = Atomic.subtract (unC gauge)

-- | Set the gauge to the given value.
set :: Gauge -> Int -> IO ()
set gauge = Atomic.write (unC gauge)
