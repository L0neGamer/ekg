{-# LANGUAGE OverloadedStrings #-}

-- | Example program that continously computes the mean of a list of
-- numbers.
module Main where

import Control.Concurrent
import Control.Exception
import Data.List
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified System.Metrics.Distribution as Distribution
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Label as Label
import System.Remote.Monitoring

-- 'sum' is using a non-strict lazy fold and will blow the stack.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: Fractional a => [a] -> a
mean xs = sum' xs / fromIntegral (length xs)

main :: IO ()
main = do
    handle <- forkServer "0.0.0.0" 8000
    baseCounter  <- getCounter "iterations" [] handle
    fizzCounter  <- getCounter "iterations" ["dim0:fizz"] handle
    buzzCounter  <- getCounter "iterations" ["dim0:buzz"] handle
    fizzbuzzCounter  <- getCounter "iterations" ["dim0:fizzbuzz"] handle
    label <- getLabel "args" [] handle
    event <- getDistribution "runtime" ["123", "456"] handle
    Label.set label "some text string"
    let counter n
            | n `mod` 15 == 0 = fizzbuzzCounter
            | n `mod` 5  == 0 = fizzCounter
            | n `mod` 3  == 0 = buzzCounter
            | otherwise       = baseCounter
    let loop n m = do
            t <- timed $ evaluate $ mean $ fmap fromInteger [1..n]
            Distribution.add event t
            threadDelay 200
            Counter.inc $ counter m
            loop n (m + 1)
    loop (1000000 :: Integer) 0

timed :: IO a -> IO Double
timed m = do
    start <- getTime
    m
    end <- getTime
    return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
