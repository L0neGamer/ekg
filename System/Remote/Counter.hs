{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
-- | This module defines a type for mutable, integer-valued counters.
-- Counters are non-negative, monotonically increasing values and can
-- be used to track e.g. the number of requests served since program
-- start.  All operations on counters are thread-safe.
module System.Remote.Counter
    (
      Counter
    , inc
    , add
    ) where

import qualified Data.Atomic as Atomic
import System.Remote.Counter.Internal

-- | Increase the counter by one.
inc :: Counter -> IO ()
inc counter = add counter 1

add :: Counter -> Int -> IO ()
add counter = Atomic.add (unC counter)
