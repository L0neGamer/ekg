{-# OPTIONS_HADDOCK not-home #-}

-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
--
-- N.B. This module exists to maintain backwards compatibility with
-- older versions of this library. New code should use the
-- @System.Metrics.Counter@ module from the ekg-core package instead.
module System.Remote.Counter
    (
      Counter.Counter
    , Counter.inc
    , Counter.add
    ) where

import qualified System.Metrics.Counter as Counter
