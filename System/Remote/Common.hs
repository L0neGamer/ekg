{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module System.Remote.Common
    (
      -- * Types
      Counters
    , Gauges
    , Labels
    , Server(..)

    , Ref(..)

      -- * User-defined counters, gauges, and labels
      -- $userdefined
    , getCounter
    , getGauge
    , getLabel

    , buildMany
    , buildAll
    , buildCombined
    , buildOne

    , parseHttpAccept
    ) where

import Control.Concurrent (ThreadId)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Encode as A
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, atomicModifyIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import qualified GHC.Stats as Stats
import Prelude hiding (read)

import System.Remote.Counter (Counter)
import qualified System.Remote.Counter.Internal as Counter
import System.Remote.Gauge (Gauge)
import qualified System.Remote.Gauge.Internal as Gauge
import System.Remote.Label (Label)
import qualified System.Remote.Label.Internal as Label

------------------------------------------------------------------------

-- Map of user-defined counters.
type Counters = M.HashMap T.Text Counter

-- Map of user-defined gauges.
type Gauges = M.HashMap T.Text Gauge

-- Map of user-defined labels.
type Labels = M.HashMap T.Text Label

-- | A handle that can be used to control the monitoring server.
-- Created by 'forkServer'.
data Server = Server {
      threadId :: {-# UNPACK #-} !ThreadId
    , userCounters :: !(IORef Counters)
    , userGauges :: !(IORef Gauges)
    , userLabels :: !(IORef Labels)
    }

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

-- $userdefined
-- The monitoring server can store and serve user-defined,
-- integer-valued counters and gauges, and string-value labels.  A
-- counter is a monotonically increasing value (e.g. TCP connections
-- established since program start.) A gauge is a variable value
-- (e.g. the current number of concurrent connections.) A label is a
-- free-form string value (e.g. exporting the command line arguments
-- or host name.)  Each counter, gauge, and label is associated with a
-- name, which is used when it is displayed in the UI or returned in a
-- JSON object.
--
-- Even though it's technically possible to have a counter and a gauge
-- with the same name, associated with the same server, it's not
-- recommended as it might make it harder for clients to distinguish
-- the two.
--
-- To create and use a counter, simply call 'getCounter' to create it
-- and then call e.g. 'System.Remote.Counter.inc' or
-- 'System.Remote.Counter.add' to modify its value.  Example:
--
-- > main = do
-- >     handle <- forkServer "localhost" 8000
-- >     counter <- getCounter "iterations" handle
-- >     let loop n = do
-- >             inc counter
-- >             loop
-- >     loop
--
-- To create a gauge, use 'getGauge' instead of 'getCounter' and then
-- call e.g. 'System.Remote.Gauge.set' or
-- 'System.Remote.Gauge.modify'.  Similar for labels.

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

-- | Return the counter associated with the given name and server.
-- Multiple calls to 'getCounter' with the same arguments will return
-- the same counter.  The first time 'getCounter' is called for a
-- given name and server, a new, zero-initialized counter will be
-- returned.
getCounter :: T.Text  -- ^ Counter name
           -> Server  -- ^ Server that will serve the counter
           -> IO Counter
getCounter name server = getRef name (userCounters server)

-- | Return the gauge associated with the given name and server.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge.  The first time 'getGauge' is called for a given
-- name and server, a new, zero-initialized gauge will be returned.
getGauge :: T.Text  -- ^ Gauge name
         -> Server  -- ^ Server that will serve the gauge
         -> IO Gauge
getGauge name server = getRef name (userGauges server)

-- | Return the label associated with the given name and server.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label.  The first time 'getLabel' is called for a given
-- name and server, a new, empty label will be returned.
getLabel :: T.Text  -- ^ Label name
         -> Server  -- ^ Server that will serve the label
         -> IO Label
getLabel name server = getRef name (userLabels server)

------------------------------------------------------------------------
-- * JSON serialization

-- | All the stats exported by the server (i.e. GC stats plus user
-- defined counters).
data Stats = Stats
    !Stats.GCStats          -- GC statistics
    ![(T.Text, Json)]       -- Counters
    ![(T.Text, Json)]       -- Gauges
    ![(T.Text, Json)]       -- Labels
    {-# UNPACK #-} !Double  -- Milliseconds since epoch

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
#if MIN_VERSION_base(4,6,0)
    , parTotBytesCopied      = 0
#else
    , parAvgBytesCopied      = 0
#endif
    , parMaxBytesCopied      = 0
    }

instance A.ToJSON Stats where
    toJSON (Stats gcStats counters gauges labels t) = A.object $
        [ "server_timestamp_millis" .= t
        , "counters"                .= Assocs (gcCounters ++ counters)
        , "gauges"                  .= Assocs (gcGauges ++ gauges)
        , "labels"                  .= Assocs (labels)
        ]
      where
        (gcCounters, gcGauges) = partitionGcStats gcStats

-- | 'Stats' encoded as a flattened JSON object.
newtype Combined = Combined Stats

instance A.ToJSON Combined where
    toJSON (Combined (Stats (Stats.GCStats {..}) counters gauges labels t)) =
        A.object $
        [ "server_timestamp_millis"  .= t
        , "bytes_allocated"          .= bytesAllocated
        , "num_gcs"                  .= numGcs
        , "max_bytes_used"           .= maxBytesUsed
        , "num_bytes_usage_samples"  .= numByteUsageSamples
        , "cumulative_bytes_used"    .= cumulativeBytesUsed
        , "bytes_copied"             .= bytesCopied
        , "current_bytes_used"       .= currentBytesUsed
        , "current_bytes_slop"       .= currentBytesSlop
        , "max_bytes_slop"           .= maxBytesSlop
        , "peak_megabytes_allocated" .= peakMegabytesAllocated
        , "mutator_cpu_seconds"      .= mutatorCpuSeconds
        , "mutator_wall_seconds"     .= mutatorWallSeconds
        , "gc_cpu_seconds"           .= gcCpuSeconds
        , "gc_wall_seconds"          .= gcWallSeconds
        , "cpu_seconds"              .= cpuSeconds
        , "wall_seconds"             .= wallSeconds
#if MIN_VERSION_base(4,6,0)
        , "par_tot_bytes_copied"     .= parTotBytesCopied
        , "par_avg_bytes_copied"     .= parTotBytesCopied
#else
        , "par_avg_bytes_copied"     .= parAvgBytesCopied
#endif
        , "par_max_bytes_copied"     .= parMaxBytesCopied
        ] ++
        map (uncurry (.=)) counters ++
        map (uncurry (.=)) gauges ++
        map (uncurry (.=)) labels

-- | A list of string keys and JSON-encodable values.  Used to render
-- a list of key-value pairs as a JSON object.
newtype Assocs = Assocs [(T.Text, Json)]

instance A.ToJSON Assocs where
    toJSON (Assocs xs) = A.object $ map (uncurry (.=)) xs

-- | A group of either counters or gauges.
data Group = Group
     ![(T.Text, Json)]
    {-# UNPACK #-} !Double  -- Milliseconds since epoch

instance A.ToJSON Group where
    toJSON (Group xs t) =
        A.object $ ("server_timestamp_millis" .= t) : map (uncurry (.=)) xs

------------------------------------------------------------------------

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r)
            -> IO [(T.Text, Json)]
readAllRefs mapRef = do
    m <- readIORef mapRef
    forM (M.toList m) $ \ (name, ref) -> do
        val <- read ref
        return (name, Json val)
{-# INLINABLE readAllRefs #-}


-- Existential wrapper used for OO-style polymorphism.
data Json = forall a. A.ToJSON a => Json a

instance A.ToJSON Json where
    toJSON (Json x) = A.toJSON x

-- | Partition GC statistics into counters and gauges.
partitionGcStats :: Stats.GCStats -> ([(T.Text, Json)], [(T.Text, Json)])
partitionGcStats (Stats.GCStats {..}) = (counters, gauges)
  where
    counters = [
          ("bytes_allocated"          , Json bytesAllocated)
        , ("num_gcs"                  , Json numGcs)
        , ("num_bytes_usage_samples"  , Json numByteUsageSamples)
        , ("cumulative_bytes_used"    , Json cumulativeBytesUsed)
        , ("bytes_copied"             , Json bytesCopied)
        , ("mutator_cpu_seconds"      , Json mutatorCpuSeconds)
        , ("mutator_wall_seconds"     , Json mutatorWallSeconds)
        , ("gc_cpu_seconds"           , Json gcCpuSeconds)
        , ("gc_wall_seconds"          , Json gcWallSeconds)
        , ("cpu_seconds"              , Json cpuSeconds)
        , ("wall_seconds"             , Json wallSeconds)
        ]
    gauges = [
          ("max_bytes_used"           , Json maxBytesUsed)
        , ("current_bytes_used"       , Json currentBytesUsed)
        , ("current_bytes_slop"       , Json currentBytesSlop)
        , ("max_bytes_slop"           , Json maxBytesSlop)
        , ("peak_megabytes_allocated" , Json peakMegabytesAllocated)
#if MIN_VERSION_base(4,6,0)
        , ("par_tot_bytes_copied"     , Json parTotBytesCopied)
        , ("par_avg_bytes_copied"     , Json parTotBytesCopied)
#else
        , ("par_avg_bytes_copied"     , Json parAvgBytesCopied)
#endif
        , ("par_max_bytes_copied"     , Json parMaxBytesCopied)
        ]

------------------------------------------------------------------------

-- | Serve a collection of counters or gauges, as a JSON object.
buildMany :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r)
    -> IO L.ByteString
buildMany mapRef = do
    list <- readAllRefs mapRef
    time <- getTimeMillis
    return $ A.encode $ A.toJSON $ Group list time
{-# INLINABLE buildMany #-}

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
buildAll :: IORef Counters -> IORef Gauges -> IORef Labels -> IO L.ByteString
buildAll counters gauges labels = do
    gcStats <- getGcStats
    counterList <- readAllRefs counters
    gaugeList <- readAllRefs gauges
    labelList <- readAllRefs labels
    time <- getTimeMillis
    return $ A.encode $ A.toJSON $ Stats gcStats counterList gaugeList
        labelList time

buildCombined :: IORef Counters -> IORef Gauges -> IORef Labels -> IO L.ByteString
buildCombined counters gauges labels = do
    gcStats <- getGcStats
    counterList <- readAllRefs counters
    gaugeList <- readAllRefs gauges
    labelList <- readAllRefs labels
    time <- getTimeMillis
    return $ A.encode $ A.toJSON $ Combined $
        Stats gcStats counterList gaugeList labelList time

buildOne :: (Ref r t, Show t)
    => IORef (M.HashMap T.Text r) -> T.Text
    -> IO (Maybe S.ByteString)
buildOne refs name = do
    m <- readIORef refs
    case M.lookup name m of
        Just counter -> do
            val <- read counter
            return $ Just $ S8.pack $ show val
        Nothing ->
            -- Try built-in (e.g. GC) refs
            case Map.lookup name builtinCounters of
                Just f -> do
                    gcStats <- liftIO getGcStats
                    return $ Just $ S8.pack $ f gcStats
                Nothing -> return Nothing
{-# INLINABLE buildOne #-}

getGcStats :: IO Stats.GCStats
getGcStats = do
#if MIN_VERSION_base(4,6,0)
    enabled <- Stats.getGCStatsEnabled
    if enabled
        then Stats.getGCStats
        else return emptyGCStats
#else
    Stats.getGCStats
#endif

-- | A list of all built-in (e.g. GC) counters, together with a
-- pretty-printing function for each.
builtinCounters :: Map.Map T.Text (Stats.GCStats -> String)
builtinCounters = Map.fromList [
      ("bytes_allocated"          , show . Stats.bytesAllocated)
    , ("num_gcs"                  , show . Stats.numGcs)
    , ("max_bytes_used"           , show . Stats.maxBytesUsed)
    , ("num_bytes_usage_samples"  , show . Stats.numByteUsageSamples)
    , ("cumulative_bytes_used"    , show . Stats.cumulativeBytesUsed)
    , ("bytes_copied"             , show . Stats.bytesCopied)
    , ("current_bytes_used"       , show . Stats.currentBytesUsed)
    , ("current_bytes_slop"       , show . Stats.currentBytesSlop)
    , ("max_bytes_slop"           , show . Stats.maxBytesSlop)
    , ("peak_megabytes_allocated" , show . Stats.peakMegabytesAllocated)
    , ("mutator_cpu_seconds"      , show . Stats.mutatorCpuSeconds)
    , ("mutator_wall_seconds"     , show . Stats.mutatorWallSeconds)
    , ("gc_cpu_seconds"           , show . Stats.gcCpuSeconds)
    , ("gc_wall_seconds"          , show . Stats.gcWallSeconds)
    , ("cpu_seconds"              , show . Stats.cpuSeconds)
    , ("wall_seconds"             , show . Stats.wallSeconds)
#if MIN_VERSION_base(4,6,0)
    , ("par_tot_bytes_copied"     , show . Stats.parTotBytesCopied)
    , ("par_avg_bytes_copied"     , show . Stats.parTotBytesCopied)
#else
    , ("par_avg_bytes_copied"     , show . Stats.parAvgBytesCopied)
#endif
    , ("par_max_bytes_copied"     , show . Stats.parMaxBytesCopied)
    ]

------------------------------------------------------------------------

-- Utilities for working with accept headers

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = List.map fst
                . List.sortBy (rcompare `on` snd)
                . List.map grabQ
                . S.split 44 -- comma
  where
    rcompare :: Double -> Double -> Ordering
    rcompare = flip compare
    grabQ s =
        let (s', q) = breakDiscard 59 s -- semicolon
            (_, q') = breakDiscard 61 q -- equals sign
         in (trimWhite s', readQ $ trimWhite q')
    readQ s = case reads $ S8.unpack s of
                (x, _):_ -> x
                _ -> 1.0
    trimWhite = S.dropWhile (== 32) -- space

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

------------------------------------------------------------------------
-- Utilities for working with timestamps

-- | Return the number of milliseconds since epoch.
getTimeMillis :: IO Double
getTimeMillis = (realToFrac . (* 1000)) `fmap` getPOSIXTime
