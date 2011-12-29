{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, integer-valued counters.
module System.Remote.Counter
    (
      Counter
    , inc
    , dec
    , add
    , subtract
    , set
    ) where

import Data.IORef (atomicModifyIORef)
import Prelude hiding (subtract)

import System.Remote.Counter.Internal

-- | Increase the counter by one.
inc :: Counter -> IO ()
inc (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + 1 in (n', n')
    return ()

-- | Decrease the counter by one.
dec :: Counter -> IO ()
dec (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - 1 in (n', n')
    return ()

-- | Increase the counter by the given amount.
add :: Counter -> Int -> IO ()
add (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + i in (n', n')
    return ()

-- | Decrease the counter by the given amount.
subtract :: Counter -> Int -> IO ()
subtract (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - i in (n', n')
    return ()

-- | Set the counter to the given value.
set :: Counter -> Int -> IO ()
set (C ref) !i = atomicModifyIORef ref $ \ _ -> (i, ())
