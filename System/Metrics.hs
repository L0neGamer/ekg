{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | A module for defining metrics that can be monitored.
--
-- Metrics are used to monitor program behavior and performance. All
-- metrics have
--
--  * a name, and
--
--  * a way to get the metric's current value.
--
-- This module provides a way to register metrics in a global \"metric
-- store\". The store can then be used to get a snapshot of all
-- metrics. The store also serves as a central place to keep track of
-- all the program's metrics, user and library defined.
--
-- Here's an example of creating a single counter of the number of
-- request served by a web server:
--
-- > import System.Metrics
-- > import qualified System.Metrics.Counter as Counter
-- >
-- > main = do
-- >     store <- newStore
-- >     requests <- createCounter "myapp.request_count" store
-- >     -- Every time we receive a request:
-- >     Counter.inc requests
--
-- This module also provides a way to register a number of predefined
-- metrics that are useful in most applications. See e.g.
-- 'registerGcStats'.
module System.Metrics
    (
      -- * Naming metrics
      -- $naming

      -- * The metric store
      -- $metric-store
      Store
    , newStore

      -- * Registering metrics
      -- $registering
    , registerCounter
    , registerGauge
    , registerLabel
    , registerGroup

      -- ** Convenience functions
      -- $convenience
    , createCounter
    , createGauge
    , createLabel

      -- ** Predefined metrics
      -- $predefined
    , registerGcStats

      -- * Sampling metrics
      -- $sampling
    , Sample
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

-- $naming
-- Compound metric names should be separated using underscores.
-- Example: @request_count@. Periods in the name imply namespacing.
-- Example: \"myapp.users\". Some consumers of metrics will use these
-- namespaces to group metrics in e.g. UIs.
--
-- Libraries and frameworks that want to register their own metrics
-- should prefix them with a namespace, to avoid collision with
-- user-defined metrics and metrics defined by other libraries. For
-- example, the Snap web framework could prefix all its metrics with
-- @\"snap.\"@.

------------------------------------------------------------------------
-- * The metric store

-- $metric-store
-- The metric store is a shared store of metrics. It allows several
-- disjoint components (e.g. libraries) to contribute to the set of
-- metrics exposed by an application. Libraries who want to provide a
-- set of metrics should defined a register method, in the style of
-- 'registerGcStats' that registers the metrics in a 'Store'. The
-- register function should document which metrics are registered and
-- their types (i.e. counter, gauge, or label).

-- | A mutable metric store.
newtype Store = Store { storeState :: IORef State }

type GroupId = Int

-- | The 'Store' state.
data State = State
     { stateMetrics :: !(M.HashMap T.Text (Either MetricSampler GroupId))
     , stateGroups  :: !(IM.IntMap GroupSampler)
     , stateNextId  :: {-# UNPACK #-} !Int
     }

data GroupSampler = forall a. GroupSampler
     { groupSampleAction   :: !(IO a)
     , groupSamplerMetrics :: !(M.HashMap T.Text (a -> Value))
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
-- metric store. The same metric name can only be used once. Passing a
-- metric name that has already been used to one of the register
-- function is an 'error'.

-- | Register a non-negative, monotonically increasing, integer-valued
-- metric. The provided action to read the value must be thread-safe.
registerCounter :: T.Text  -- ^ Metric name
                -> IO Int  -- ^ Action to read the current metric value
                -> Store   -- ^ Metric store
                -> IO ()
registerCounter name sample store =
    register name (CounterS sample) store

-- | Register an integer-valued metric. The provided action to read
-- the value must be thread-safe.
registerGauge :: T.Text  -- ^ Metric name
              -> IO Int  -- ^ Action to read the current metric value
              -> Store   -- ^ Metric store
              -> IO ()
registerGauge name sample store =
    register name (GaugeS sample) store

-- | Register a text metric. The provided action to read the value
-- must be thread-safe.
registerLabel :: T.Text     -- ^ Metric name
              -> IO T.Text  -- ^ Action to read the current metric value
              -> Store      -- ^ Metric store
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

-- | Raise an exception indicating that the metric name is already in
-- use.
alreadyInUseError :: T.Text -> a
alreadyInUseError name =
    error $ "The name \"" ++ show name ++ "\" is already taken " ++
    "by a metric."

-- | Register a action that will be executed any time one of the
-- metrics computed from the value it returns needs to be sampled.
--
-- When one or more of the metrics listed in the first argument needs
-- to be sampled, the action is executed and the provided getter
-- functions will be used to extract the metric(s) from the action's
-- return value.
--
-- The registered action might be called from a different thread and
-- therefore needs to be thread-safe.
--
-- This function allows you to sample groups of metrics together. This
-- is useful if
--
-- * you need a consistent view of several metric or
--
-- * because sampling the metrics together is more efficient.
--
-- For example, sampling GC statistics needs to be done atomically or
-- a GC might strike in the middle of sampling, rendering the values
-- incoherent. Sampling GC statistics is also more efficient if done
-- in one step, as the run-time system provides a function to sample
-- all GC statistics at once.
--
-- Note that sampling of the metrics is only atomic if the action
-- computes @a@ atomically (e.g. if @a@ is a record, the action needs
-- to compute its fields atomically if the sampling is to be atomic.)
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
-- >     registerGroup (M.fromList metrics) getGCStats store
registerGroup
    :: M.HashMap T.Text
       (a -> Value)  -- ^ Metric names and getter functions.
    -> IO a          -- ^ Action to sample the metric group
    -> Store         -- ^ Metric store
    -> IO ()
registerGroup getters cb store = do
    atomicModifyIORef (storeState store) $ \ State{..} ->
        let !state' = State
                { stateMetrics = M.foldlWithKey' (register_ stateNextId)
                                 stateMetrics getters
                , stateGroups  = IM.insert stateNextId
                                 (GroupSampler cb getters)
                                 stateGroups
                , stateNextId  = stateNextId + 1
                }
       in (state', ())
  where
    register_ groupId metrics name _ = case M.lookup name metrics of
        Nothing -> M.insert name (Right groupId) metrics
        Just _  -> alreadyInUseError name

------------------------------------------------------------------------
-- ** Convenience functions

-- $convenience
-- These functions combined the creation of a mutable reference (e.g.
-- a 'Counter') with registering that reference in the store in one
-- convenient function.

-- | Create and register a zero-initialized counter.
createCounter :: T.Text  -- ^ Counter name
              -> Store   -- ^ Metric store
              -> IO Counter
createCounter name store = do
    counter <- Counter.new
    registerCounter name (Counter.read counter) store
    return counter

-- | Create and register a zero-initialized gauge.
createGauge :: T.Text  -- ^ Gauge name
            -> Store   -- ^ Metric store
            -> IO Gauge
createGauge name store = do
    gauge <- Gauge.new
    registerGauge name (Gauge.read gauge) store
    return gauge

-- | Create and register an empty label.
createLabel :: T.Text  -- ^ Label name
            -> Store   -- ^ Metric store
            -> IO Label
createLabel name store = do
    label <- Label.new
    registerLabel name (Label.read label) store
    return label

------------------------------------------------------------------------
-- * Predefined metrics

-- $predefined
-- This library provides a number of pre-defined metrics that can
-- easily be added to a metrics store by calling their register
-- function.

-- | Convert seconds to milliseconds.
toMs :: Double -> Int
toMs s = round (s * 1000.0)

-- | Register a number of metrics related to garbage collector
-- behavior.
--
-- Registerd counters:
--
-- [@rts.gc.bytes_allocated@] Total number of bytes allocated
--
-- [@rts.gc.num_gcs@] Number of garbage collections performed
--
-- [@rts.gc.num_bytes_usage_samples@] Number of byte usage samples taken
--
-- [@rts.gc.cumulative_bytes_used@] Sum of all byte usage samples, can be
-- used with @numByteUsageSamples@ to calculate averages with
-- arbitrary weighting (if you are sampling this record multiple
-- times).
--
-- [@rts.gc.bytes_copied@] Number of bytes copied during GC
--
-- [@rts.gc.mutator_cpu_ms@] CPU time spent running mutator threads,
-- in milliseconds. This does not include any profiling overhead or
-- initialization.
--
-- [@rts.gc.mutator_wall_ms@] Wall clock time spent running mutator
-- threads, in milliseconds. This does not include initialization.
--
-- [@rts.gc.gc_cpu_ms@] CPU time spent running GC, in milliseconds.
--
-- [@rts.gc.gc_wall_ms@] Wall clock time spent running GC, in
-- milliseconds.
--
-- [@rts.gc.cpu_ms@] Total CPU time elapsed since program start, in
-- milliseconds.
--
-- [@rts.gc.wall_ms@] Total wall clock time elapsed since start, in
-- milliseconds.
--
-- Registered gauges:
--
-- [@rts.gc.max_bytes_used@] Maximum number of live bytes seen so far
--
-- [@rts.gc.current_bytes_used@] Current number of live bytes
--
-- [@rts.gc.current_bytes_slop@] Current number of bytes lost to slop
--
-- [@rts.gc.max_bytes_slop@] Maximum number of bytes lost to slop at any one time so far
--
-- [@rts.gc.peak_megabytes_allocated@] Maximum number of megabytes allocated
--
-- [@rts.gc.par_tot_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@rts.gc.par_avg_bytes_copied@] Deprecated alias for
-- @par_tot_bytes_copied@.
--
-- [@rts.gc.par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC. The ratio of
-- @par_tot_bytes_copied@ divided by @par_max_bytes_copied@ approaches
-- 1 for a maximally sequential run and approaches the number of
-- threads (set by the RTS flag @-N@) for a maximally parallel run.
registerGcStats :: Store -> IO ()
registerGcStats store =
    registerGroup
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

-- | Get GC statistics.
getGcStats :: IO Stats.GCStats
#if MIN_VERSION_base(4,6,0)
getGcStats = do
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats

-- | Empty GC statistics, as if the application hasn't started yet.
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

------------------------------------------------------------------------
-- * Sampling metrics

-- $sampling
-- The metrics register in the store can be sampled together. Sampling
-- is /not/ atomic. While each metric will be retrieved atomically,
-- the sample is not an atomic snapshot of the system as a whole. See
-- 'registerGroup' for an explanation of how to sample a subset of all
-- metrics atomically.

-- | A sample of some metrics.
type Sample = M.HashMap T.Text Value

-- | Sample all metrics. Sampling is /not/ atomic in the sense that
-- some metrics might have been mutated before they're sampled but
-- after some other metrics have already been sampled.
sampleAll :: Store -> IO Sample
sampleAll store = do
    time <- getTimeMs
    state <- readIORef (storeState store)
    let metrics = stateMetrics state
        groups = stateGroups state
    cbSample <- sampleGroups $ IM.elems groups
    sample <- readAllRefs metrics
    let allSamples = sample ++ cbSample ++
                     [("server_timestamp_ms", Counter time)]
    return $! M.fromList allSamples
  where
    getTimeMs :: IO Int
    getTimeMs = (round . (* 1000)) `fmap` getPOSIXTime

-- | Sample all metric groups.
sampleGroups :: [GroupSampler] -> IO [(T.Text, Value)]
sampleGroups cbSamplers = concat `fmap` sequence (map runOne cbSamplers)
  where
    runOne :: GroupSampler -> IO [(T.Text, Value)]
    runOne GroupSampler{..} = do
        a <- groupSampleAction
        return $! map (\ (n, f) -> (n, f a)) (M.toList groupSamplerMetrics)

-- | The value of a sampled metric.
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
readAllRefs :: M.HashMap T.Text (Either MetricSampler GroupId)
            -> IO [(T.Text, Value)]
readAllRefs m = do
    forM ([(name, ref) | (name, Left ref) <- M.toList m]) $ \ (name, ref) -> do
        val <- sampleOne ref
        return (name, val)
