{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module System.Remote.Monitoring where

import qualified Data.Aeson.Encode as A
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as L
import GHC.Stats

instance A.ToJSON GCStats where
    toJSON (GCStats {..}) = A.object [
          "bytes_allocated" .= bytesAllocated
        , "num_gcs" .= numGcs
        , "max_bytes_used" .= maxBytesUsed
        , "num_bytes_usage_samples" .= numByteUsageSamples
        , "cumulative_butes_used" .= cumulativeBytesUsed
        , "bytes_copied" .= bytesCopied
        , "current_bytes_used" .= currentBytesUsed
        , "current_bytes_slop" .= currentBytesSlop
        , "max_bytes_slop" .= maxBytesSlop
        , "peak_megabytes_allocated" .= peakMegabytesAllocated
        , "mutator_cpu_seconds" .= mutatorCpuSeconds
        , "mutator_wall_seconds" .= mutatorWallSeconds
        , "gc_cpu_seconds" .= gcCpuSeconds
        , "gc_wall_seconds" .= gcWallSeconds
        , "cpu_seconds" .= cpuSeconds
        , "wall_seconds" .= wallSeconds
        , "par_avg_bytes_copied" .= parAvgBytesCopied
        , "par_max_bytes_copied" .= parMaxBytesCopied
        ]

getStatsAsJson :: IO L.ByteString
getStatsAsJson = (A.encode . A.toJSON) `fmap` getGCStats
