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
    , modify
    ) where

import Data.IORef (atomicModifyIORef)
import Prelude hiding (subtract)

import System.Remote.Gauge.Internal

-- | Increase the gauge by one.
inc :: Gauge -> IO ()
inc (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + 1 in (n', n')
    return ()

-- | Decrease the gauge by one.
dec :: Gauge -> IO ()
dec (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - 1 in (n', n')
    return ()

-- | Increase the gauge by the given amount.
add :: Gauge -> Int -> IO ()
add (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + i in (n', n')
    return ()

-- | Decrease the gauge by the given amount.
subtract :: Gauge -> Int -> IO ()
subtract (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - i in (n', n')
    return ()

-- | Set the gauge to the given value.
set :: Gauge -> Int -> IO ()
set (C ref) !i = atomicModifyIORef ref $ \ _ -> (i, ())

-- | Set the gauge to the result of applying the given function to the
-- value.
modify :: (Int -> Int) -> Gauge -> IO ()
modify f (C ref) = do
    !_ <- atomicModifyIORef ref $ \ i -> let i' = f i in (i', i')
    return ()
