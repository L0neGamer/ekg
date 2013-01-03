{-# LANGUAGE CPP #-}

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

import Control.Concurrent (ThreadId, forkIO)
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as M
import Data.IORef (newIORef)
import Prelude hiding (read)

import System.Remote.Common

import System.Remote.Snap

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
    tid <- forkIO $ startServer counters gauges labels host port
    return $! Server tid counters gauges labels
