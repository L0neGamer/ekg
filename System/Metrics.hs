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

newtype Store = Store { metricMaps :: IORef MetricMaps }

data MetricMaps = MetricMaps
    { userCounters :: !Counters
    , userGauges   :: !Gauges
    , userLabels   :: !Labels
    }

setUserCounters :: MetricMaps -> Counters -> MetricMaps
setUserCounters maps counters = maps { userCounters = counters }

setUserGauges :: MetricMaps -> Gauges -> MetricMaps
setUserGauges maps gauges = maps { userGauges = gauges }

setUserLabels :: MetricMaps -> Labels -> MetricMaps
setUserLabels maps labels = maps { userLabels = labels }

newStore :: IO Store
newStore = do
    maps <- newIORef $ MetricMaps
        { userCounters = M.empty
        , userGauges = M.empty
        , userLabels = M.empty
        }
    return $ Store maps

-- Map of counters.
type Counters = M.HashMap T.Text (IO Int)

-- Map of gauges.
type Gauges = M.HashMap T.Text (IO Int)

-- Map of labels.
type Labels = M.HashMap T.Text (IO T.Text)

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

register :: T.Text
         -> IO r
         -> (MetricMaps -> M.HashMap T.Text (IO r))
         -> (MetricMaps -> M.HashMap T.Text (IO r) -> MetricMaps)
         -> Store
         -> IO ()
register name sample get set store = do
    atomicModifyIORef (metricMaps store) $ \ maps ->
        if inUse name maps
        then alreadyInUseError name maps
        else -- Guaranteed to not be in map at this point.
             let !m = get maps
             in let !m'    = M.insert name sample m
                    !maps' = set maps m'
                in (maps', ())

alreadyInUseError :: T.Text -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

inUse :: T.Text -> MetricMaps -> Bool
inUse name MetricMaps{..} = name `M.member` userCounters ||
                            name `M.member` userGauges ||
                            name `M.member` userLabels

registerCounter :: T.Text -> IO Int -> Store -> IO ()
registerCounter name sample store =
    register name sample userCounters setUserCounters store

registerGauge :: T.Text -> IO Int -> Store -> IO ()
registerGauge name sample store =
    register name sample userGauges setUserGauges store

registerLabel :: T.Text -> IO T.Text -> Store -> IO ()
registerLabel name sample store =
    register name sample userLabels setUserLabels store

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
data Metrics = Metrics
    { metricsCounters :: !(M.HashMap T.Text Int)
    , metricsGauges   :: !(M.HashMap T.Text Int)
    , metricsLabels   :: !(M.HashMap T.Text T.Text)
    } deriving Show

-- | Sample all metrics.
sampleAll :: Store -> IO Metrics
sampleAll store = do
    time <- getTimeMs
    MetricMaps{..} <- readIORef $ metricMaps store
    counters <- readAllRefs userCounters
    gauges <- readAllRefs userGauges
    labels <- readAllRefs userLabels
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

sampleCombined :: Store -> IO (M.HashMap T.Text Metric)
sampleCombined store = do
    metrics <- sampleAll store
    -- This assumes that the same name wasn't used for two different
    -- metric types.
    return $! M.unions [M.map Counter (metricsCounters metrics),
                        M.map Gauge (metricsGauges metrics),
                        M.map Label (metricsLabels metrics)]

sampleCounters :: Store -> IO (M.HashMap T.Text Int)
sampleCounters store = metricsCounters <$> sampleAll store

sampleCounter :: T.Text -> Store -> IO (Maybe Int)
sampleCounter name store = do
    counters <- sampleCounters store
    return $! M.lookup name counters

sampleGauges :: Store -> IO (M.HashMap T.Text Int)
sampleGauges store = metricsGauges <$> sampleAll store

sampleGauge :: T.Text -> Store -> IO (Maybe Int)
sampleGauge name store = do
    gauges <- sampleGauges store
    return $! M.lookup name gauges

sampleLabels :: Store -> IO (M.HashMap T.Text T.Text)
sampleLabels store = metricsLabels <$> sampleAll store

sampleLabel :: T.Text -> Store -> IO (Maybe T.Text)
sampleLabel name store = do
    labels <- sampleLabels store
    return $! M.lookup name labels

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: M.HashMap T.Text (IO t) -> IO [(T.Text, t)]
readAllRefs m = do
    forM (M.toList m) $ \ (name, sample) -> do
        val <- sample
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
