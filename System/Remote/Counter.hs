{-# LANGUAGE BangPatterns #-}
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

import Data.IORef (atomicModifyIORef)
import Prelude hiding (subtract)

import System.Remote.Counter.Internal

-- | Increase the counter by one.
inc :: Counter -> IO ()
inc (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + 1 in (n', n')
    return ()

-- | Increase the counter by the given amount.
add :: Counter -> Int -> IO ()
add (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + i in (n', n')
    return ()
