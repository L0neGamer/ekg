{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Metrics
    (
      -- * Types
      MetricStore
    , newMetricStore

      -- * User-defined counters, gauges, and labels
    , getCounter
    , getGauge
    , getLabel

      -- * Sampling
    , Metrics(..)
    , sampleAll
    , Metric(..)
    , sampleCombined
    , sampleCounters
    , sampleCounter
    , sampleGauges
    , sampleGauge
    , sampleLabels
    , sampleLabel
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

data MetricStore = MetricStore
    { userCounters :: !(IORef Counters)
    , userGauges   :: !(IORef Gauges)
    , userLabels   :: !(IORef Labels)
    }

newMetricStore :: IO MetricStore
newMetricStore = do
    counters <- newIORef M.empty
    gauges <- newIORef M.empty
    labels <- newIORef M.empty
    return $ MetricStore counters gauges labels


-- Map of user-defined counters.
type Counters = M.HashMap T.Text Counter

-- Map of user-defined gauges.
type Gauges = M.HashMap T.Text Gauge

-- Map of user-defined labels.
type Labels = M.HashMap T.Text Label

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

class Ref r t | r -> t where
    new :: IO r
    read :: r -> IO t

instance Ref Counter Int where
    new = Counter.new
    read = Counter.read

instance Ref Gauge Int where
    new = Gauge.new
    read = Gauge.read

instance Ref Label T.Text where
    new = Label.new
    read = Label.read

-- | Lookup a 'Ref' by name in the given map.  If no 'Ref' exists
-- under the given name, create a new one, insert it into the map and
-- return it.
getRef :: Ref r t
       => T.Text                      -- ^ 'Ref' name
       -> IORef (M.HashMap T.Text r)  -- ^ Server that will serve the 'Ref'
       -> IO r
getRef name mapRef = do
    empty <- new
    ref <- atomicModifyIORef mapRef $ \ m ->
        case M.lookup name m of
            Nothing  -> let m' = M.insert name empty m
                        in (m', empty)
            Just ref -> (m, ref)
    return ref
{-# INLINABLE getRef #-}

-- | Return the counter associated with the given name and metric
-- store. Multiple calls to 'getCounter' with the same arguments will
-- return the same counter. The first time 'getCounter' is called for
-- a given name and metric store, a new, zero-initialized counter will
-- be returned.
getCounter :: T.Text       -- ^ Counter name
           -> MetricStore  -- ^ The metric store
           -> IO Counter
getCounter name store = getRef name (userCounters store)

-- | Return the gauge associated with the given name and metric store.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge. The first time 'getGauge' is called for a given
-- name and metric store, a new, zero-initialized gauge will be
-- returned.
getGauge :: T.Text       -- ^ Gauge name
         -> MetricStore  -- ^ The metric store
         -> IO Gauge
getGauge name store = getRef name (userGauges store)

-- | Return the label associated with the given name and metric store.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label. The first time 'getLabel' is called for a given
-- name and metric store, a new, empty label will be returned.
getLabel :: T.Text       -- ^ Label name
         -> MetricStore  -- ^ The metric store
         -> IO Label
getLabel name store = getRef name (userLabels store)

------------------------------------------------------------------------
-- * Sampling

-- | A sample of some metrics.
data Metrics = Metrics
    { metricsCounters :: !(M.HashMap T.Text Int)
    , metricsGauges   :: !(M.HashMap T.Text Int)
    , metricsLabels   :: !(M.HashMap T.Text T.Text)
    }

-- | Sample all metrics.
sampleAll :: MetricStore -> IO Metrics
sampleAll store = do
    time <- getTimeMs
    counters <- readAllRefs (userCounters store)
    gauges <- readAllRefs (userGauges store)
    labels <- readAllRefs (userLabels store)
    (gcCounters, gcGauges) <- partitionGcStats <$> getGcStats
    let allCounters = counters ++ gcCounters ++ [("server_timestamp_ms", time)]
        allGauges   = gauges ++ gcGauges
    return $! Metrics
        (M.fromList allCounters)
        (M.fromList allGauges)
        (M.fromList labels)
  where
    getTimeMs :: IO Int
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

-- | The kind of metrics that can be tracked.
data Metric = Counter {-# UNPACK #-} !Int
            | Gauge {-# UNPACK #-} !Int
            | Label {-# UNPACK #-} !T.Text

sampleCombined :: MetricStore -> IO (M.HashMap T.Text Metric)
sampleCombined store = do
    metrics <- sampleAll store
    -- This assumes that the same name wasn't used for two different
    -- metric types.
    return $! M.unions [M.map Counter (metricsCounters metrics),
                        M.map Gauge (metricsGauges metrics),
                        M.map Label (metricsLabels metrics)]

sampleCounters :: MetricStore -> IO (M.HashMap T.Text Int)
sampleCounters store = metricsCounters <$> sampleAll store

sampleCounter :: T.Text -> MetricStore -> IO (Maybe Int)
sampleCounter name store = do
    counters <- sampleCounters store
    return $! M.lookup name counters

sampleGauges :: MetricStore -> IO (M.HashMap T.Text Int)
sampleGauges store = metricsGauges <$> sampleAll store

sampleGauge :: T.Text -> MetricStore -> IO (Maybe Int)
sampleGauge name store = do
    gauges <- sampleGauges store
    return $! M.lookup name gauges

sampleLabels :: MetricStore -> IO (M.HashMap T.Text T.Text)
sampleLabels store = metricsLabels <$> sampleAll store

sampleLabel :: T.Text -> MetricStore -> IO (Maybe T.Text)
sampleLabel name store = do
    labels <- sampleLabels store
    return $! M.lookup name labels

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: Ref r t => IORef (M.HashMap T.Text r) -> IO [(T.Text, t)]
readAllRefs mapRef = do
    m <- readIORef mapRef
    forM (M.toList m) $ \ (name, ref) -> do
        val <- read ref
        return (name, val)
{-# INLINABLE readAllRefs #-}

------------------------------------------------------------------------
-- Built-in GC stats

-- | Convert seconds to milliseconds.
toMs :: Double -> Int
toMs s = round (s * 1000.0)

-- | Partition GC statistics into counters and gauges.
partitionGcStats :: Stats.GCStats -> ([(T.Text, Int)], [(T.Text, Int)])
partitionGcStats s@(Stats.GCStats {..}) = (counters, gauges)
  where
    counters = [
          ("rts.gc.bytes_allocated"          , int bytesAllocated)
        , ("rts.gc.num_gcs"                  , int numGcs)
        , ("rts.gc.num_bytes_usage_samples"  , int numByteUsageSamples)
        , ("rts.gc.cumulative_bytes_used"    , int cumulativeBytesUsed)
        , ("rts.gc.bytes_copied"             , int bytesCopied)
        , ("rts.gc.mutator_cpu_ms"           , toMs mutatorCpuSeconds)
        , ("rts.gc.mutator_wall_ms"          , toMs mutatorWallSeconds)
        , ("rts.gc.gc_cpu_ms"                , toMs gcCpuSeconds)
        , ("rts.gc.gc_wall_ms"               , toMs gcWallSeconds)
        , ("rts.gc.cpu_ms"                   , toMs cpuSeconds)
        , ("rts.gc.wall_ms"                  , toMs wallSeconds)
        ]
    gauges = [
          ("rts.gc.max_bytes_used"           , int maxBytesUsed)
        , ("rts.gc.current_bytes_used"       , int currentBytesUsed)
        , ("rts.gc.current_bytes_slop"       , int currentBytesSlop)
        , ("rts.gc.max_bytes_slop"           , int maxBytesSlop)
        , ("rts.gc.peak_megabytes_allocated" , int peakMegabytesAllocated)
        , ("rts.gc.par_tot_bytes_copied"     , int (gcParTotBytesCopied s))
        , ("rts.gc.par_avg_bytes_copied"     , int (gcParTotBytesCopied s))
        , ("rts.gc.par_max_bytes_copied"     , int parMaxBytesCopied)
        ]
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
