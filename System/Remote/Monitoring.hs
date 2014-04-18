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

      -- * Security considerations
      -- $security

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
import qualified Data.Text as T
import Prelude hiding (read)

import qualified System.Metrics as Metrics
import System.Remote.Counter (Counter)
import System.Remote.Gauge (Gauge)
import System.Remote.Label (Label)
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

-- $security
-- Be aware that if the server started by 'forkServer' is not bound to
-- \"localhost\" (or equivalent) anyone on the network can access the
-- monitoring server. Either make sure the network is secure or bind
-- the server to \"localhost\".

-- $api
-- To use the machine-readable REST API, send an HTTP GET request to
-- the host and port passed to 'forkServer'.
--
-- The API is versioned to allow for API evolution. This document is
-- for version 1. To ensure you're using this version, append @?v=1@
-- to your resource URLs. Omitting the version number will give you
-- the latest version of the API.
--
-- The following resources (i.e. URLs) are available:
--
-- [\/] JSON object containing all counters, gauges and labels.
-- Counters, gauges, and labels are stored as nested objects under the
-- @counters@, @gauges@, and @labels@ attributes, respectively.
-- Content types: \"text\/html\" (default), \"application\/json\"
--
-- [\/\<namespace\>/\<metric\>] JSON object for a single metric. The
-- metric name is created by converting all \/ to \".\". Example:
-- \"\/foo\/bar\" corresponds to the metric \"foo.bar\". Content
-- types: \"application\/json\"
--
-- Each metric is returned as an object containing a @val@ and a
-- @type@ type. The @val@ field contains the actual value (i.e. an
-- integer or a string) and the @type@ field specifies the metric
-- type. Available types are:
--
--  * \"c\" - 'System.Counter'
--
--  * \"g\" - 'System.Gauge'
--
--  * \"l\" - 'System.Label'
--
-- Example of a response containing the metrics \"myapp.visitors\" and
-- \"myapp.args\":
--
-- > {
-- >   "myapp": {
-- >     "visitors": {
-- >       "val": 10,
-- >       "type": "c"
-- >     },
-- >     "args": {
-- >       "val": "--a-flag",
-- >       "type": "l"
-- >     }
-- >   }
-- > }
--
-- In addition to user-defined counters, gauges, and labels, the below
-- built-in counters and gauges are also returned.
--
-- Built-in counters:
--
-- [@server_time_ms@] The server time when the sample was taken, in
-- milliseconds.
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
-- Built-in gauges:
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

-- $userdefined
-- The monitoring server can store and serve user-defined,
-- integer-valued counters and gauges, and string-valued labels.  A
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
    store <- Metrics.newStore
    Metrics.registerGCStats store
    tid <- forkIO $ startServer store host port
    return $! Server tid store

------------------------------------------------------------------------
-- * Types

-- | A handle that can be used to control the monitoring server.
-- Created by 'forkServer'.
data Server = Server {
      threadId :: {-# UNPACK #-} !ThreadId
    , metricStore :: {-# UNPACK #-} !Metrics.Store
    }

------------------------------------------------------------------------
-- * User-defined counters, gauges and labels

-- | Return the counter associated with the given name and server.
-- Multiple calls to 'getCounter' with the same arguments will return
-- the same counter.  The first time 'getCounter' is called for a
-- given name and server, a new, zero-initialized counter will be
-- returned.
getCounter :: T.Text  -- ^ Counter name
           -> Server  -- ^ Server that will serve the counter
           -> IO Counter
getCounter name server = Metrics.createCounter name (metricStore server)

-- | Return the gauge associated with the given name and server.
-- Multiple calls to 'getGauge' with the same arguments will return
-- the same gauge.  The first time 'getGauge' is called for a given
-- name and server, a new, zero-initialized gauge will be returned.
getGauge :: T.Text  -- ^ Gauge name
         -> Server  -- ^ Server that will serve the gauge
         -> IO Gauge
getGauge name server = Metrics.createGauge name (metricStore server)

-- | Return the label associated with the given name and server.
-- Multiple calls to 'getLabel' with the same arguments will return
-- the same label.  The first time 'getLabel' is called for a given
-- name and server, a new, empty label will be returned.
getLabel :: T.Text  -- ^ Label name
         -> Server  -- ^ Server that will serve the label
         -> IO Label
getLabel name server = Metrics.createLabel name (metricStore server)
