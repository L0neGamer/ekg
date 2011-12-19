{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Allows for remote monitoring of a running executable over HTTP.
--
-- This module can be used to run an HTTP server (or a Snap handler)
-- that replies to HTTP requests with either an HTML page or a JSON
-- object.  The former can be used by a human to get an overview of a
-- program's GC stats and the latter can be used be automated tools.
--
-- Typical usage is to start the monitor server on program startup
--
-- > main = do
-- >     forkServer "localhost:8000"
-- >     ...
--
-- and then periodically check the stats using a browser or using a
-- command line tool like curl
--
-- > $ curl -H "Accept: application/json" http://localhost:8000/
module System.Remote.Monitoring
    (
      -- * Required configuration
      -- $configuration

      -- * JSON API
      -- $api
      forkServer
    , monitor
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import qualified Data.Aeson.Encode as A
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function
import Data.List as List
import Data.Word
import GHC.Stats
import Paths_ekg
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import System.FilePath

-- $configuration
--
-- To use this module you must first enable GC stats collection in
-- your program.  To do that, either run your program with
--
-- > +RTS -T
--
-- or compile it with
--
-- > -with-rtsopts=-T
--
-- The overhead from `-T` is very small so it's safe to always leave
-- it enabled.

-- $api
--
-- The HTTP server replies to GET requests to the URL passed to
-- 'forkServer'.  The client can choose the desired response
-- Content-Type by setting the Accept header to either \"text\/html\"
-- or \"application\/json\".  If set to \"application\/json\", the
-- server returns a JSON object with the following members:
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

-- | Run an HTTP server, that exposes GC stats, in a new thread.  The
-- server replies to requests on the given host and port
-- (e.g. "localhost:8000").  You can kill the server by killing the
-- thread (i.e. by throwing it an asynchronous exception.)
forkServer :: String -> IO ThreadId
forkServer _addr = forkIO $ quickHttpServe monitor

-- Orphan instance
instance A.ToJSON GCStats where
    toJSON (GCStats {..}) = A.object
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
        ]

-- | A handler that can be installed into an existing Snap application.
monitor :: Snap ()
monitor = do
    dataDir <- liftIO getDataDir
    route [ ("/", index) ]
        <|> serveDirectory (dataDir </> "public")

index :: Snap ()
index = do
    req <- getRequest
    let acceptHdr = maybe "application/json" (List.head . parseHttpAccept) $
                    getAcceptHeader req
    if acceptHdr == "application/json"
        then serveJson
        else pass
  where
    serveJson = do
        modifyResponse $ setContentType "application/json"
        stats <- liftIO getGCStats
        writeLBS $ A.encode $ A.toJSON $ stats

    getAcceptHeader :: Request -> Maybe S.ByteString
    getAcceptHeader req = S.intercalate "," <$> getHeaders "Accept" req

------------------------------------------------------------------------
-- Utilities for working with accept headers

-- | Parse the HTTP accept string to determine supported content types.
parseHttpAccept :: S.ByteString -> [S.ByteString]
parseHttpAccept = List.map fst
                . sortBy (rcompare `on` snd)
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
