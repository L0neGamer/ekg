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
    handle <- forkServer "localhost" 8000
    counter <- getCounter "iterations" handle
    label <- getLabel "args" handle
    event <- getDistribution "runtime" handle
    Label.set label "some text string"
    let loop n = do
            t <- timed $ evaluate $ mean [1..n]
            Distribution.add event t
            threadDelay 2000
            Counter.inc counter
            loop n
    loop 1000000

timed :: IO a -> IO Double
timed m = do
    start <- getTime
    m
    end <- getTime
    return $! end - start

getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime
