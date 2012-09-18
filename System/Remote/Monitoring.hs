{-# LANGUAGE CPP, ExistentialQuantification, OverloadedStrings, RecordWildCards,
  FunctionalDependencies #-}
-- | This module provides remote monitoring of a running process over
-- HTTP.  It can be used to run an HTTP server that provides both a
-- web-based user interface and a machine-readable API (e.g. JSON.)
-- The former can be used by a human to get an overview of what the
-- program is doing and the latter can be used by automated monitoring
-- tools.
--
-- Typical usage is to start the monitoring server at program startup
--
-- > main = do
-- >     forkServer "localhost" 8000
-- >     ...
--
-- and then periodically check the stats using a web browser or a
-- command line tool (e.g. curl)
--
-- > $ curl -H "Accept: application/json" http://localhost:8000/
module System.Remote.Monitoring
    (
      -- * Required configuration
      -- $configuration

      -- * REST API
      -- $api

      -- * The monitoring server
      Server
    , serverThreadId
    , forkServer

      -- * User-defined counters, gauges, and labels
      -- $userdefined
    , getCounter
    , getGauge
    , getLabel
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forM, join, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Encode as A
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word8)
import qualified GHC.Stats as Stats
import Paths_ekg (getDataDir)
import Prelude hiding (read)
import Snap.Core (MonadSnap, Request, Snap, finishWith, getHeaders, getRequest,
                  getResponse, method, Method(GET), modifyResponse, pass, route,
                  rqParams, rqPathInfo, setContentType, setResponseStatus,
                  writeBS, writeLBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Config
import Snap.Util.FileServe (serveDirectory)
import System.FilePath ((</>))

import System.Remote.Counter (Counter)
import qualified System.Remote.Counter.Internal as Counter
import System.Remote.Gauge (Gauge)
import qualified System.Remote.Gauge.Internal as Gauge
import System.Remote.Label (Label)
import qualified System.Remote.Label.Internal as Label

-- $configuration
--
-- To use this module you must first enable GC statistics collection
-- in the run-time system.  To enable GC statistics collection, either
-- run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The runtime overhead of @-T@ is very small so it's safe to always
-- leave it enabled.

-- $api
-- To use the machine-readable REST API, send an HTTP GET request to
-- the host and port passed to 'forkServer'.  The following resources
-- (i.e. URLs) are available:
--
-- [\/] JSON object containing all counters and gauges.  Counters and
-- gauges are stored as nested objects under the @counters@ and
-- @gauges@ attributes, respectively.  Content types: \"text\/html\"
-- (default), \"application\/json\"
--
-- [\/combined] Flattened JSON object containing all counters, gauges,
-- and labels.  Content types: \"application\/json\"
--
-- [\/counters] JSON object containing all counters.  Content types:
-- \"application\/json\"
--
-- [\/counters/\<counter name\>] Value of a single counter, as a
-- string.  The name should be UTF-8 encoded.  Content types:
-- \"text\/plain\"
--
-- [\/gauges] JSON object containing all gauges.  Content types:
-- \"application\/json\"
--
-- [\/gauges/\<gauge name\>] Value of a single gauge, as a string.
-- The name should be UTF-8 encoded.  Content types: \"text\/plain\"
--
-- [\/labels] JSON object containing all labels.  Content types:
-- \"application\/json\"
--
-- [\/labels/\<label name\>] Value of a single label, as a string.
-- The name should be UTF-8 encoded.  Content types: \"text\/plain\"
--
-- Counters, gauges and labels are stored as attributes of the
-- returned JSON objects, one attribute per counter, gauge or label.
-- In addition to user-defined counters, gauges, and labels, the below
-- built-in counters and gauges are also returned.  Furthermore, the
-- top-level JSON object of any resource contains the
-- @server_timestamp_millis@ attribute, which indicates the server
-- time, in milliseconds, when the sample was taken.
--
-- Built-in counters:
--
-- [@bytes_allocated@] Total number of bytes allocated
--
-- [@num_gcs@] Number of garbage collections performed
--
-- [@num_bytes_usage_samples@] Number of byte usage samples taken
--
-- [@cumulative_bytes_used@] Sum of all byte usage samples, can be
-- used with @numByteUsageSamples@ to calculate averages with
-- arbitrary weighting (if you are sampling this record multiple
-- times).
--
-- [@bytes_copied@] Number of bytes copied during GC
--
-- [@mutator_cpu_seconds@] CPU time spent running mutator threads.
-- This does not include any profiling overhead or initialization.
--
-- [@mutator_wall_seconds@] Wall clock time spent running mutator
-- threads.  This does not include initialization.
--
-- [@gc_cpu_seconds@] CPU time spent running GC
--
-- [@gc_wall_seconds@] Wall clock time spent running GC
--
-- [@cpu_seconds@] Total CPU time elapsed since program start
--
-- [@wall_seconds@] Total wall clock time elapsed since start
--
-- Built-in gauges:
--
-- [@max_bytes_used@] Maximum number of live bytes seen so far
--
-- [@current_bytes_used@] Current number of live bytes
--
-- [@current_bytes_slop@] Current number of bytes lost to slop
--
-- [@max_bytes_slop@] Maximum number of bytes lost to slop at any one time so far
--
-- [@peak_megabytes_allocated@] Maximum number of megabytes allocated
--
#if MIN_VERSION_base(4,6,0)
-- [@par_tot_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@par_avg_bytes_copied@] Deprecated alias for
-- @par_tot_bytes_copied@.
#else
-- [@par_avg_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
#endif
--
-- [@par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC.  The ratio of
#if MIN_VERSION_base(4,6,0)
-- 'parTotBytesCopied' divided by 'parMaxBytesCopied' approaches 1 for
#else
-- 'parAvgBytesCopied' divided by 'parMaxBytesCopied' approaches 1 for
#endif
-- a maximally sequential run and approaches the number of threads
-- (set by the RTS flag @-N@) for a maximally parallel run.

------------------------------------------------------------------------
-- * The monitoring server

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

-- | The thread ID of the server.  You can kill the server by killing
-- this thread (i.e. by throwing it an asynchronous exception.)
serverThreadId :: Server -> ThreadId
serverThreadId = threadId

-- | Start an HTTP server in a new thread.  The server replies to GET
-- requests to the given host and port.  The host argument can be
-- either a numeric network address (dotted quad for IPv4,
-- colon-separated hex for IPv6) or a hostname (e.g. \"localhost\".)
-- The client can control the Content-Type used in responses by
-- setting the Accept header.  At the moment three content types are
-- available: \"application\/json\", \"text\/html\", and
-- \"text\/plain\".
forkServer :: S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
           -> Int           -- ^ Port to listen on (e.g. 8000)
           -> IO Server
forkServer host port = do
    counters <- newIORef M.empty
    gauges <- newIORef M.empty
    labels <- newIORef M.empty
    tid <- forkIO $ httpServe conf (monitor counters gauges labels)
    return $! Server tid counters gauges labels
  where conf = Config.setVerbose False $
               Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               Config.setHostname host $
               Config.defaultConfig

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

instance A.ToJSON Stats where
    toJSON (Stats gcStats@(Stats.GCStats {..}) counters gauges labels t) = A.object $
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
        ] ++ map (uncurry (.=)) counters ++
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
-- * HTTP request handler

-- | A handler that can be installed into an existing Snap application.
monitor :: IORef Counters -> IORef Gauges -> IORef Labels -> Snap ()
monitor counters gauges labels = do
    dataDir <- liftIO getDataDir
    route [
          ("", method GET (format "application/json"
                           (serveAll counters gauges labels)))
        , ("combined", method GET (format "application/json"
                                   (serveCombined counters gauges labels)))
        , ("counters", method GET (format "application/json"
                                   (serveMany counters)))
        , ("counters/:name", method GET (format "text/plain"
                                         (serveOne counters)))
        , ("gauges", method GET (format "application/json"
                                 (serveMany gauges)))
        , ("gauges/:name", method GET (format "text/plain"
                                       (serveOne gauges)))
        , ("labels", method GET (format "application/json"
                                 (serveMany labels)))
        , ("labels/:name", method GET (format "text/plain"
                                       (serveOne labels)))
        ]
        <|> serveDirectory (dataDir </> "assets")

-- | The Accept header of the request.
acceptHeader :: Request -> Maybe S.ByteString
acceptHeader req = S.intercalate "," <$> getHeaders "Accept" req

-- | Runs a Snap monad action only if the request's Accept header
-- matches the given MIME type.
format :: MonadSnap m => S.ByteString -> m a -> m a
format fmt action = do
    req <- getRequest
    let acceptHdr = (List.head . parseHttpAccept) <$> acceptHeader req
    case acceptHdr of
        Just hdr | hdr == fmt -> action
        _ -> pass

-- | Get a snapshot of all values.  Note that we're not guaranteed to
-- see a consistent snapshot of the whole map.
readAllRefs :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r) -> IO [(T.Text, Json)]
readAllRefs mapRef = do
    m <- readIORef mapRef
    forM (M.toList m) $ \ (name, ref) -> do
        val <- read ref
        return (name, Json val)
{-# INLINABLE readAllRefs #-}

-- | Serve a collection of counters or gauges, as a JSON object.
serveMany :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r) -> Snap ()
serveMany mapRef = do
    list <- liftIO $ readAllRefs mapRef
    modifyResponse $ setContentType "application/json"
    time <- liftIO getTimeMillis
    writeLBS $ A.encode $ A.toJSON $ Group list time
{-# INLINABLE serveMany #-}

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
serveAll :: IORef Counters -> IORef Gauges -> IORef Labels -> Snap ()
serveAll counters gauges labels = do
    req <- getRequest
    -- Workaround: Snap still matches requests to /foo to this handler
    -- if the Accept header is "application/json", even though such
    -- requests ought to go to the 'serveOne' handler.
    unless (S.null $ rqPathInfo req) pass
    modifyResponse $ setContentType "application/json"
    gcStats <- liftIO Stats.getGCStats
    counterList <- liftIO $ readAllRefs counters
    gaugeList <- liftIO $ readAllRefs gauges
    labelList <- liftIO $ readAllRefs labels
    time <- liftIO getTimeMillis
    writeLBS $ A.encode $ A.toJSON $ Stats gcStats counterList gaugeList labelList time

-- | Serve all counters and gauges, built-in or not, as a flattened
-- JSON object.
serveCombined :: IORef Counters -> IORef Gauges -> IORef Labels -> Snap ()
serveCombined counters gauges labels = do
    modifyResponse $ setContentType "application/json"
    gcStats <- liftIO Stats.getGCStats
    counterList <- liftIO $ readAllRefs counters
    gaugeList <- liftIO $ readAllRefs gauges
    labelList <- liftIO $ readAllRefs labels
    time <- liftIO getTimeMillis
    writeLBS $ A.encode $ A.toJSON $ Combined $
        Stats gcStats counterList gaugeList labelList time

-- | Serve a single counter, as plain text.
serveOne :: (Ref r t, Show t) => IORef (M.HashMap T.Text r) -> Snap ()
serveOne refs = do
    modifyResponse $ setContentType "text/plain"
    m <- liftIO $ readIORef refs
    req <- getRequest
    let mname = T.decodeUtf8 <$> join
                (listToMaybe <$> Map.lookup "name" (rqParams req))
    case mname of
        Nothing -> pass
        Just name -> case M.lookup name m of
            Just counter -> do
                val <- liftIO $ read counter
                writeBS $ S8.pack $ show val
            Nothing ->
                -- Try built-in (e.g. GC) refs
                case Map.lookup name builtinCounters of
                    Just f -> do
                        gcStats <- liftIO Stats.getGCStats
                        writeBS $ S8.pack $ f gcStats
                    Nothing -> do
                        modifyResponse $ setResponseStatus 404 "Not Found"
                        r <- getResponse
                        finishWith r
{-# INLINABLE serveOne #-}

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

-- Existential wrapper used for OO-style polymorphism.
data Json = forall a. A.ToJSON a => Json a

instance A.ToJSON Json where
    toJSON (Json x) = A.toJSON x

-- | Partition GC statistics into counters and gauges.
partitionGcStats :: Stats.GCStats
                 -> ([(T.Text, Json)], [(T.Text, Json)])
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
