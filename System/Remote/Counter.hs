-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
module System.Remote.Counter
    (
      Counter.Counter
    , Counter.inc
    , Counter.add
    ) where

import qualified System.Metrics.Counter as Counter
