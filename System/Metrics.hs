{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
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

      -- ** Built-in metrics
    , registerGCStats

      -- * Sampling metrics
      -- $sampling
    , Metrics
    , sampleAll
    , Value(..)
    ) where

import Control.Applicative ((<$>))
import Control.Monad (forM)
import Data.Int (Int64)
import qualified Data.IntMap.Strict as IM
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
-- * The metric store

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

type CallbackId = Int

data State = State
     { stateMetrics   :: !(M.HashMap T.Text (Either MetricSampler CallbackId))
     , stateCallbacks :: !(IM.IntMap CallbackSampler)
     , stateNextId    :: {-# UNPACK #-} !Int
     }

data CallbackSampler = forall a. CallbackSampler
     { metricCallback :: IO a
     , metricGetters  :: M.HashMap T.Text (a -> Value)
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
                                              (Left sample)
                                              stateMetrics
                             }
                     in (state', ())
            True  -> alreadyInUseError name

alreadyInUseError :: T.Text -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

-- TODO: We might want to have the callback return the values sampled.
-- That way we don't need to allocate and maintain 'IORef's for all
-- metrics sampled through a callback (e.g. GC metrics).

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
-- > import qualified Data.HashMap.Strict as M
-- > import GHC.Stats
-- >
-- > main = do
-- >     store <- newStore
-- >     let metrics =
-- >         [ ("num_gcs", numGcs)
-- >         , ("max_bytes_used", maxBytesUsed)]
-- >         ]
-- >     registerCallback (M.fromList metrics) getGCStats store
registerCallback
    :: M.HashMap T.Text (a -> Value)  -- ^ Metric names and
                                      -- projection functions.
    -> IO a                           -- ^ The metrics sampler
    -> Store                          -- ^ The metric store
    -> IO ()
registerCallback getters cb store = do
    atomicModifyIORef (storeState store) $ \ State{..} ->
        let !state' = State
                { stateMetrics = M.foldlWithKey' (register_ stateNextId)
                                 stateMetrics getters
                , stateCallbacks = IM.insert stateNextId
                                   (CallbackSampler cb getters)
                                   stateCallbacks
                , stateNextId    = stateNextId + 1
                }
       in (state', ())
  where
    register_ cbId metrics name _ = case M.lookup name metrics of
        Nothing -> M.insert name (Right cbId) metrics
        Just _  -> alreadyInUseError name

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
type Metrics = M.HashMap T.Text Value

-- | Sample all metrics.
sampleAll :: Store -> IO Metrics
sampleAll store = do
    time <- getTimeMs
    state <- readIORef (storeState store)
    let metrics = stateMetrics state
        callbacks = stateCallbacks state
    cbSample <- sampleCallbacks $ IM.elems callbacks
    sample <- readAllRefs metrics
    let allSamples = sample ++ cbSample ++
                     [("server_timestamp_ms", Counter time)]
    return $! M.fromList allSamples
  where
    getTimeMs :: IO Int
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

sampleCallbacks :: [CallbackSampler] -> IO [(T.Text, Value)]
sampleCallbacks cbSamplers = concat `fmap` sequence (map runOne cbSamplers)
  where
    runOne :: CallbackSampler -> IO [(T.Text, Value)]
    runOne CallbackSampler{..} = do
        a <- metricCallback
        return $! map (\ (n, f) -> (n, f a)) (M.toList metricGetters)

-- | The kind of metrics that can be sampled.
data Value = Counter {-# UNPACK #-} !Int
            | Gauge {-# UNPACK #-} !Int
            | Label {-# UNPACK #-} !T.Text
            deriving (Eq, Show)

sampleOne :: MetricSampler -> IO Value
sampleOne (CounterS m) = Counter <$> m
sampleOne (GaugeS m)   = Gauge <$> m
sampleOne (LabelS m)   = Label <$> m

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: M.HashMap T.Text (Either MetricSampler CallbackId)
            -> IO [(T.Text, Value)]
readAllRefs m = do
    forM ([(name, ref) | (name, Left ref) <- M.toList m]) $ \ (name, ref) -> do
        val <- sampleOne ref
        return (name, val)
{-# INLINABLE readAllRefs #-}

------------------------------------------------------------------------
-- Built-in GC stats

-- | Convert seconds to milliseconds.
toMs :: Double -> Int
toMs s = round (s * 1000.0)

registerGCStats :: Store -> IO ()
registerGCStats store =
    registerCallback
    (M.fromList
     [ ("rts.gc.bytes_allocated"          , Counter . int . Stats.bytesAllocated)
     , ("rts.gc.num_gcs"                  , Counter . int . Stats.numGcs)
     , ("rts.gc.num_bytes_usage_samples"  , Counter . int . Stats.numByteUsageSamples)
     , ("rts.gc.cumulative_bytes_used"    , Counter . int . Stats.cumulativeBytesUsed)
     , ("rts.gc.bytes_copied"             , Counter . int . Stats.bytesCopied)
     , ("rts.gc.mutator_cpu_ms"           , Counter . toMs . Stats.mutatorCpuSeconds)
     , ("rts.gc.mutator_wall_ms"          , Counter . toMs . Stats.mutatorWallSeconds)
     , ("rts.gc.gc_cpu_ms"                , Counter . toMs . Stats.gcCpuSeconds)
     , ("rts.gc.gc_wall_ms"               , Counter . toMs . Stats.gcWallSeconds)
     , ("rts.gc.cpu_ms"                   , Counter . toMs . Stats.cpuSeconds)
     , ("rts.gc.wall_ms"                  , Counter . toMs . Stats.wallSeconds)
     , ("rts.gc.max_bytes_used"           , Gauge . int . Stats.maxBytesUsed)
     , ("rts.gc.current_bytes_used"       , Gauge . int . Stats.currentBytesUsed)
     , ("rts.gc.current_bytes_slop"       , Gauge . int . Stats.currentBytesSlop)
     , ("rts.gc.max_bytes_slop"           , Gauge . int . Stats.maxBytesSlop)
     , ("rts.gc.peak_megabytes_allocated" , Gauge . int . Stats.peakMegabytesAllocated)
     , ("rts.gc.par_tot_bytes_copied"     , Gauge . int . gcParTotBytesCopied)
     , ("rts.gc.par_avg_bytes_copied"     , Gauge . int . gcParTotBytesCopied)
     , ("rts.gc.par_max_bytes_copied"     , Gauge . int . Stats.parMaxBytesCopied)
     ])
    getGcStats
    store
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
