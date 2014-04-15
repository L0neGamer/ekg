{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Metrics
    (
      -- * Types
      Store
    , newStore

      -- * User-defined counters, gauges, and labels
    , getCounter
    , getGauge
    , getLabel
    , registerCounter
    , registerGauge
    , registerLabel

      -- * Sampling
    , Metrics
    , sampleAll
    , Metric(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Remote.Counter (Counter)
import qualified System.Remote.Counter.Internal as Counter
import System.Remote.Gauge (Gauge)
import qualified System.Remote.Gauge.Internal as Gauge
import System.Remote.Label (Label)
import qualified System.Remote.Label.Internal as Label

------------------------------------------------------------------------
-- * Types

newtype Store = Store { metricMaps :: IORef (M.HashMap T.Text MetricSampler) }

-- TODO: Rename this to Metric and Metric to SampledMetric.
data MetricSampler = CounterS !(IO Int)
                   | GaugeS !(IO Int)
                   | LabelS !(IO T.Text)

newStore :: IO Store
newStore = do
    maps <- newIORef $ M.empty
    return $ Store maps

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

register :: T.Text
         -> MetricSampler
         -> Store
         -> IO ()
register name sample store = do
    atomicModifyIORef (metricMaps store) $ \ m ->
        case M.member name m of
            False -> let !m' = M.insert name sample m
                     in (m', ())
            True  -> alreadyInUseError name

alreadyInUseError :: T.Text -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

registerCounter :: T.Text -> IO Int -> Store -> IO ()
registerCounter name sample store =
    register name (CounterS sample) store

registerGauge :: T.Text -> IO Int -> Store -> IO ()
registerGauge name sample store =
    register name (GaugeS sample) store

registerLabel :: T.Text -> IO T.Text -> Store -> IO ()
registerLabel name sample store =
    register name (LabelS sample) store

-- | Return the counter associated with the given name and metric
-- store. Multiple calls to 'getCounter' with the same arguments will
-- return the same counter. The first time 'getCounter' is called for
-- a given name and metric store, a new, zero-initialized counter will
-- be returned.
getCounter :: T.Text       -- ^ Counter name
           -> Store  -- ^ The metric store
           -> IO Counter
getCounter name store = do
    counter <- Counter.new
    registerCounter name (Counter.read counter) store
    return counter

-- | Return the gauge associated with the given name and metric store.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge. The first time 'getGauge' is called for a given
-- name and metric store, a new, zero-initialized gauge will be
-- returned.
getGauge :: T.Text       -- ^ Gauge name
         -> Store  -- ^ The metric store
         -> IO Gauge
getGauge name store = do
    gauge <- Gauge.new
    registerGauge name (Gauge.read gauge) store
    return gauge

-- | Return the label associated with the given name and metric store.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label. The first time 'getLabel' is called for a given
-- name and metric store, a new, empty label will be returned.
getLabel :: T.Text       -- ^ Label name
         -> Store  -- ^ The metric store
         -> IO Label
getLabel name store = do
    label <- Label.new
    registerLabel name (Label.read label) store
    return label

------------------------------------------------------------------------
-- * Sampling

-- | A sample of some metrics.
type Metrics = M.HashMap T.Text Metric

-- | Sample all metrics.
sampleAll :: Store -> IO Metrics
sampleAll store = do
    time <- getTimeMs
    metrics <- readIORef $ metricMaps store
    sample <- readAllRefs metrics
    gcSample <- sampleGcStats <$> getGcStats
    let allSamples = sample ++ gcSample ++
                     [("server_timestamp_ms", Counter time)]
    return $! M.fromList allSamples
  where
    getTimeMs :: IO Int
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

-- | The kind of metrics that can be tracked.
data Metric = Counter {-# UNPACK #-} !Int
            | Gauge {-# UNPACK #-} !Int
            | Label {-# UNPACK #-} !T.Text
            deriving (Eq, Show)

sampleOne :: MetricSampler -> IO Metric
sampleOne (CounterS m) = Counter <$> m
sampleOne (GaugeS m)   = Gauge <$> m
sampleOne (LabelS m)   = Label <$> m

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: M.HashMap T.Text MetricSampler -> IO [(T.Text, Metric)]
readAllRefs m = do
    forM (M.toList m) $ \ (name, sampler) -> do
        val <- sampleOne sampler
        return (name, val)
{-# INLINABLE readAllRefs #-}

------------------------------------------------------------------------
-- Built-in GC stats

-- | Convert seconds to milliseconds.
toMs :: Double -> Int
toMs s = round (s * 1000.0)

-- | Sample GC statistics into counters and gauges.
sampleGcStats :: Stats.GCStats -> [(T.Text, Metric)]
sampleGcStats s@(Stats.GCStats {..}) =
    [ ("rts.gc.bytes_allocated"          , Counter $ int bytesAllocated)
    , ("rts.gc.num_gcs"                  , Counter $ int numGcs)
    , ("rts.gc.num_bytes_usage_samples"  , Counter $ int numByteUsageSamples)
    , ("rts.gc.cumulative_bytes_used"    , Counter $ int cumulativeBytesUsed)
    , ("rts.gc.bytes_copied"             , Counter $ int bytesCopied)
    , ("rts.gc.mutator_cpu_ms"           , Counter $ toMs mutatorCpuSeconds)
    , ("rts.gc.mutator_wall_ms"          , Counter $ toMs mutatorWallSeconds)
    , ("rts.gc.gc_cpu_ms"                , Counter $ toMs gcCpuSeconds)
    , ("rts.gc.gc_wall_ms"               , Counter $ toMs gcWallSeconds)
    , ("rts.gc.cpu_ms"                   , Counter $ toMs cpuSeconds)
    , ("rts.gc.wall_ms"                  , Counter $ toMs wallSeconds)
    , ("rts.gc.max_bytes_used"           , Gauge $ int maxBytesUsed)
    , ("rts.gc.current_bytes_used"       , Gauge $ int currentBytesUsed)
    , ("rts.gc.current_bytes_slop"       , Gauge $ int currentBytesSlop)
    , ("rts.gc.max_bytes_slop"           , Gauge $ int maxBytesSlop)
    , ("rts.gc.peak_megabytes_allocated" , Gauge $ int peakMegabytesAllocated)
    , ("rts.gc.par_tot_bytes_copied"     , Gauge $ int (gcParTotBytesCopied s))
    , ("rts.gc.par_avg_bytes_copied"     , Gauge $ int (gcParTotBytesCopied s))
    , ("rts.gc.par_max_bytes_copied"     , Gauge $ int parMaxBytesCopied)
    ]
  where
    int = fromIntegral

getGcStats :: IO Stats.GCStats
#if MIN_VERSION_base(4,6,0)
getGcStats = do
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats

emptyGCStats :: Stats.GCStats
emptyGCStats = Stats.GCStats
    { bytesAllocated         = 0
    , numGcs                 = 0
    , maxBytesUsed           = 0
    , numByteUsageSamples    = 0
    , cumulativeBytesUsed    = 0
    , bytesCopied            = 0
    , currentBytesUsed       = 0
    , currentBytesSlop       = 0
    , maxBytesSlop           = 0
    , peakMegabytesAllocated = 0
    , mutatorCpuSeconds      = 0
    , mutatorWallSeconds     = 0
    , gcCpuSeconds           = 0
    , gcWallSeconds          = 0
    , cpuSeconds             = 0
    , wallSeconds            = 0
    , parTotBytesCopied      = 0
    , parMaxBytesCopied      = 0
    }
#else
getGcStats = Stats.getGCStats
#endif

-- | Helper to work around rename in GHC.Stats in base-4.6.
gcParTotBytesCopied :: Stats.GCStats -> Int64
#if MIN_VERSION_base(4,6,0)
gcParTotBytesCopied = Stats.parTotBytesCopied
#else
gcParTotBytesCopied = Stats.parAvgBytesCopied
#endif
