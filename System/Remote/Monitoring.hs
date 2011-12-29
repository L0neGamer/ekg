{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Allows for remote monitoring of a running process over HTTP.
--
-- This module can be used to run an HTTP server that replies to HTTP
-- requests with either an HTML page or a JSON object.  The former can
-- be used by a human to get an overview of a program's GC stats and
-- the latter can be used be automated tools.
--
-- Typical usage is to start the monitor server on program startup
--
-- > main = do
-- >     forkServer "localhost" 8000
-- >     ...
--
-- and then periodically check the stats using a web browser or a
-- command line tool like curl
--
-- > $ curl -H "Accept: application/json" http://localhost:8000/
module System.Remote.Monitoring
    (
      -- * Required configuration
      -- $configuration

      -- * JSON API
      -- $api

      -- * The monitor server
      Server
    , serverThreadId
    , forkServer

      -- * User defined counters
      -- $counters
    , getCounter
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forM)
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
import qualified Data.Text as T
import Data.Word (Word8)
import qualified GHC.Stats as Stats
import Paths_ekg (getDataDir)
import Snap.Core (Request, Snap, getHeaders, getRequest, modifyResponse, pass,
                  route, setContentType, writeLBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Config
import Snap.Util.FileServe (serveDirectory)
import System.FilePath ((</>))

import System.Remote.Counter (Counter)
import qualified System.Remote.Counter.Internal as Counter

-- $configuration
--
-- To use this module you must first enable GC stats collection for
-- your program.  To enable GC stats collection, either run your
-- program with
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
--
-- The HTTP server replies to GET requests to the host and port passed
-- to 'forkServer'.  To get a JSON formatted response, the client must
-- set the Accept header to \"application\/json\".  The server returns
-- a JSON object with the following members:
--
-- [@bytes_allocated@] Total number of bytes allocated
--
-- [@num_gcs@] Number of garbage collections performed
--
-- [@max_bytes_used@] Maximum number of live bytes seen so far
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
-- [@current_bytes_used@] Current number of live bytes
--
-- [@current_bytes_slop@] Current number of bytes lost to slop
--
-- [@max_bytes_slop@] Maximum number of bytes lost to slop at any one time so far
--
-- [@peak_megabytes_allocated@] Maximum number of megabytes allocated
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
-- [@par_avg_bytes_copied@] Number of bytes copied during GC, minus
-- space held by mutable lists held by the capabilities.  Can be used
-- with 'parMaxBytesCopied' to determine how well parallel GC utilized
-- all cores.
--
-- [@par_max_bytes_copied@] Sum of number of bytes copied each GC by
-- the most active GC thread each GC.  The ratio of
-- 'parAvgBytesCopied' divided by 'parMaxBytesCopied' approaches 1 for
-- a maximally sequential run and approaches the number of threads
-- (set by the RTS flag @-N@) for a maximally parallel run.

------------------------------------------------------------------------
-- * The monitor server

-- Map of user defined counters.
type Counters = M.HashMap T.Text Counter

-- | A handle that can be used to control the monitor server.  Created
-- by 'forkServer'.
data Server = Server {
      threadId :: {-# UNPACK #-} !ThreadId
    , userCounters :: !(IORef Counters)
    }

-- | The thread ID of the server.  You can kill the server by killing
-- this thread (i.e. by throwing it an asynchronous exception.)
serverThreadId :: Server -> ThreadId
serverThreadId = threadId

-- | Start an HTTP server in a new thread.  The server replies to GET
-- requests to the given host and port.  The host argument can be
-- either a numeric network address (dotted quad for IPv4,
-- colon-separated hex for IPv6) or a hostname (e.g. \"localhost\").
-- The client can set the desired response format (i.e. Content-Type)
-- by setting the Accept header.  At the moment two response formats
-- are available: \"application\/json\" and \"text\/html\".
forkServer :: S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
           -> Int           -- ^ Port to listen on (e.g. 8000)
           -> IO Server
forkServer host port = do
    counters <- newIORef M.empty
    tid <- forkIO $ httpServe conf (monitor counters)
    return $! Server tid counters
  where conf = Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               Config.setHostname host $
               Config.defaultConfig

------------------------------------------------------------------------
-- * User defined counters

-- $counters
-- The monitor server can store and serve user defined integer-valued
-- counters.  Each counter is associated with a name, which will be
-- used when the counter is displayed in the UI or returned as part of
-- the JSON API.  It's recommended to group related counters by
-- prefixing their name with a dot-separated namespace
-- (e.g. \"mygroup.mycounter\".)
--
-- To create and use a counter, simply call 'getCounter' to create it
-- and then call e.g. 'Counter.inc' or 'Counter.add' to modify its
-- value.

-- | Return the counter associated with the given name.  Multiple
-- calls to 'getCounter' with the same name will return the same
-- counter.  The first time 'getCounter' is called for a given name, a
-- zero initialized counter will be returned.
getCounter :: T.Text  -- ^ Counter name
           -> Server  -- ^ Server to serve counters
           -> IO Counter
getCounter name server = do
    emptyCounter <- Counter.new
    counter <- atomicModifyIORef (userCounters server) $ \ m ->
        case M.lookup name m of
            Nothing      -> let m' = M.insert name emptyCounter m
                            in (m', emptyCounter)
            Just counter -> (m, counter)
    return counter

------------------------------------------------------------------------
-- * JSON serialization

-- All the stats exported by the server (i.e. GC stats plus user
-- defined counters).
data Stats = Stats !Stats.GCStats ![(T.Text, Int)]

instance A.ToJSON Stats where
    toJSON (Stats (Stats.GCStats {..}) cs) = A.object $
        [ "bytes_allocated"          .= bytesAllocated
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
        , "par_avg_bytes_copied"     .= parAvgBytesCopied
        , "par_max_bytes_copied"     .= parMaxBytesCopied
        ] ++ map (\ (k, v) -> k .= v) cs

------------------------------------------------------------------------
-- * HTTP request handler

-- | A handler that can be installed into an existing Snap application.
monitor :: IORef Counters -> Snap ()
monitor counters = do
    dataDir <- liftIO getDataDir
    route [ ("/", index counters) ]
        <|> serveDirectory (dataDir </> "public")

index :: IORef Counters -> Snap ()
index counters = do
    req <- getRequest
    let acceptHdr = maybe "application/json" (List.head . parseHttpAccept) $
                    getAcceptHeader req
    if acceptHdr == "application/json"
        then serveJson
        else pass
  where
    serveJson = do
        modifyResponse $ setContentType "application/json"
        gcStats <- liftIO Stats.getGCStats
        counterList <- liftIO readAllCounters
        writeLBS $ A.encode $ A.toJSON $ Stats gcStats counterList

    getAcceptHeader :: Request -> Maybe S.ByteString
    getAcceptHeader req = S.intercalate "," <$> getHeaders "Accept" req

    readAllCounters :: IO [(T.Text, Int)]
    readAllCounters = do
        m <- readIORef counters
        forM (M.toList m) $ \ (name, ref) -> do
            val <- Counter.read ref
            return (name, val)

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
