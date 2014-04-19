-- | This module defines a type for mutable, integer-valued gauges.
-- Gauges are variable values and can be used to track e.g. the
-- current number of concurrent connections. All operations on gauges
-- are thread-safe.
module System.Remote.Gauge
    (
      Gauge
    , new
    , read
    , inc
    , dec
    , add
    , subtract
    , set
    ) where

import qualified Data.Atomic as Atomic
import Prelude hiding (subtract, read)

-- | A mutable, integer-valued gauge.
newtype Gauge = C { unC :: Atomic.Atomic }

-- | Create a new, zero initialized, gauge.
new :: IO Gauge
new = C `fmap` Atomic.new 0

-- | Get the current value of the gauge.
read :: Gauge -> IO Int
read = Atomic.read . unC

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
