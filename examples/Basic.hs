{-# LANGUAGE OverloadedStrings #-}

-- | Example program that continously computes the mean of a list of
-- numbers.
module Main where

import Control.Concurrent
import Control.Exception
import qualified System.Remote.Counter as Counter
import qualified System.Remote.Label as Label
import System.Remote.Monitoring

mean :: Fractional a => [a] -> a
mean xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
    handle <- forkServer "localhost" 8000
    counter <- getCounter "iterations" handle
    label <- getLabel "args" handle
    Label.set label "some text string"
    let loop n = do
            evaluate $ mean [1..n]
            threadDelay 2000
            Counter.inc counter
            loop n
    loop 1000000
