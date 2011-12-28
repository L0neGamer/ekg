{-# LANGUAGE BangPatterns #-}
module System.Remote.Counter
    (
      new
    , inc
    , dec
    , add
    , subtract
    , set
    ) where

import Data.IORef
import Prelude hiding (subtract)

newtype Counter = C (IORef Int)

new :: IO Counter
new = undefined

inc :: Counter -> IO ()
inc (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + 1 in (n', n')
    return ()

dec :: Counter -> IO ()
dec (C ref) = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - 1 in (n', n')
    return ()

add :: Counter -> Int -> IO ()
add (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n + i in (n', n')
    return ()

subtract :: Counter -> Int -> IO ()
subtract (C ref) i = do
    !_ <- atomicModifyIORef ref $ \ n -> let n' = n - i in (n', n')
    return ()

set :: Counter -> Int -> IO ()
set (C ref) !i = atomicModifyIORef ref $ \ _ -> (i, ())
