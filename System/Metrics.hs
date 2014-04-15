{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Metrics
    (
      -- * The metric store
      Store
    , newStore

      -- * Registering metrics
      -- $registering
    , registerCounter
    , registerGauge
    , registerLabel
    , registerCallback
      -- ** Convenience functions
    , getCounter
    , getGauge
    , getLabel

      -- * Sampling metrics
      -- $sampling
    , Metrics
    , sampleAll
    , Metric(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.List as List
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
-- * The metric store

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

data State = State
     { stateMetrics   :: !(M.HashMap T.Text MetricRef)
     , stateCallbacks :: !(IM.IntMap (IO ()))
     , stateNextId    :: {-# UNPACK #-} !Int
     }

data MetricRef = MetricRef
     { metricSample   :: !MetricSampler
     , metricCallback :: !(Maybe Int)
     }

-- TODO: Rename this to Metric and Metric to SampledMetric.
data MetricSampler = CounterS !(IO Int)
                   | GaugeS !(IO Int)
                   | LabelS !(IO T.Text)

-- | Create a new, empty metric store.
newStore :: IO Store
newStore = do
    state <- newIORef $ State M.empty IM.empty 0
    return $ Store state

------------------------------------------------------------------------
-- * Registering metrics

-- $registering
-- Before metrics can be sampled they need to be registered with the
-- metric store. The same metric name must only be used once.

-- | Register a non-negative, monotonically increasing integer-valued
-- metric. The provided action to read the value must be thread-safe.
registerCounter :: T.Text  -- ^ The metric name
                -> IO Int  -- ^ Action to read the value
                -> Store   -- ^ The metric store
                -> IO ()
registerCounter name sample store =
    register name (CounterS sample) store

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe.
registerGauge :: T.Text  -- ^ The metric name
              -> IO Int  -- ^ Action to read the value
              -> Store   -- ^ The metric store
              -> IO ()
registerGauge name sample store =
    register name (GaugeS sample) store

-- | Register a text metric. The provided action to read the value
-- must be thread-safe.
registerLabel :: T.Text     -- ^ The metric name
              -> IO T.Text  -- ^ Action to read the value
              -> Store      -- ^ The metric store
              -> IO ()
registerLabel name sample store =
    register name (LabelS sample) store

register :: T.Text
         -> MetricSampler
         -> Store
         -> IO ()
register name sample store = do
    atomicModifyIORef (storeState store) $ \ state@State{..} ->
        case M.member name stateMetrics of
            False -> let !state' = state {
                               stateMetrics = M.insert name
                                              (MetricRef sample Nothing)
                                              stateMetrics
                             }
                     in (state', ())
            True  -> alreadyInUseError name

alreadyInUseError :: T.Text -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

-- | Register a callback that will be called any time one of the
-- metrics updated by the callback needs to be sampled.
--
-- Registered callbacks might be called from a different thread and
-- therefore need to be thread-safe. No more than one callback can be
-- registered per metric.
--
-- Callbacks allow you to sample groups of metrics together. This is
-- useful if you need a consistent view of several metric or because
-- sampling the metrics together is more efficient. For example,
-- sampling GC statistics needs to be done atomically or a GC might
-- strike in the middle of sampling, rendering the values incoherent.
-- Sampling GC statistics is also more efficient if done in one step,
-- as the run-time system provides a function to sample all GC
-- statistics at once.
--
-- Example usage:
--
-- > main = do
-- >     store <- newStore
-- >     temp <- newIORef 0
-- >     registerGauge "cpu_temp" (readIORef temp) store
-- >     registerCallback ["cpu_temp"] (getCpuTemp >>= writeIORef temp) store
registerCallback :: [T.Text]  -- ^ Names of metrics updated by this callback
                 -> IO ()     -- ^ The callback
                 -> Store     -- ^ The metric store
                 -> IO ()
registerCallback names cb store = do
    atomicModifyIORef (storeState store) $ \ State{..} ->
        let !state' = State
                { stateMetrics = List.foldl' (register_ stateNextId)
                                 stateMetrics names
                , stateCallbacks = IM.insert stateNextId cb stateCallbacks
                , stateNextId    = stateNextId + 1
                }
       in (state', ())
  where
    register_ cbId metrics name = case M.lookup name metrics of
        Nothing ->
            error $ "No metric named \"" ++ T.unpack name ++ "\"."
        Just (MetricRef _ (Just _)) ->
            error $ "A callback has already been associated with metric \"" ++
            T.unpack name ++ "\""
        Just (MetricRef sampler _) ->
            M.insert name (MetricRef sampler (Just cbId)) metrics

-- | Create and register a zero-initialized counter.
getCounter :: T.Text  -- ^ Counter name
           -> Store   -- ^ The metric store
           -> IO Counter
getCounter name store = do
    counter <- Counter.new
    registerCounter name (Counter.read counter) store
    return counter

-- | Create and register a zero-initialized gauge.
getGauge :: T.Text  -- ^ Gauge name
         -> Store   -- ^ The metric store
         -> IO Gauge
getGauge name store = do
    gauge <- Gauge.new
    registerGauge name (Gauge.read gauge) store
    return gauge

-- | Create and register an empty label.
getLabel :: T.Text  -- ^ Label name
         -> Store   -- ^ The metric store
         -> IO Label
getLabel name store = do
    label <- Label.new
    registerLabel name (Label.read label) store
    return label

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerCallback' for an idea how to sample a subset of all
-- metrics atomically.
--
-- Before any sampling is done, any callbacks that cover the metrics
-- being sampled are run.

-- | A sample of some metrics.
type Metrics = M.HashMap T.Text Metric

-- | Sample all metrics.
sampleAll :: Store -> IO Metrics
sampleAll store = do
    time <- getTimeMs
    state <- readIORef (storeState store)
    let metrics = stateMetrics state
        callbacks = stateCallbacks state
    sequence_ $ IM.elems callbacks
    sample <- readAllRefs metrics
    gcSample <- sampleGcStats <$> getGcStats
    let allSamples = sample ++ gcSample ++
                     [("server_timestamp_ms", Counter time)]
    return $! M.fromList allSamples
  where
    getTimeMs :: IO Int
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

-- | The kind of metrics that can be sampled.
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
readAllRefs :: M.HashMap T.Text MetricRef -> IO [(T.Text, Metric)]
readAllRefs m = do
    forM (M.toList m) $ \ (name, ref) -> do
        val <- sampleOne (metricSample ref)
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
