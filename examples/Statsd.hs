module Main
    ( main
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.List (foldl')
import System.Metrics
import System.Remote.Monitoring.Statsd

-- 'sum' is using a non-strict lazy fold and will blow the stack.
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

mean :: Fractional a => [a] -> a
mean xs = sum' xs / fromIntegral (length xs)

main :: IO ()
main = do
    store <- newMetricStore
    forkStatsd defaultStatsdOptions store
    let loop n = do
            evaluate $ mean [1..n]
            threadDelay 2000
            loop n
    loop 1000000
