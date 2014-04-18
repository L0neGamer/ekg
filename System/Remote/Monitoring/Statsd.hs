{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Remote.Monitoring.Statsd
    (
      forkStatsd
    , StatsdOptions(..)
    , defaultStatsdOptions
    , Statsd
    , statsdThreadId
    ) where

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as M
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Metrics as Metrics
import System.IO (stderr)

-- | A handle that can be used to control the statsd sync thread.
-- Created by 'forkStatsd'.
data Statsd = Statsd
    { threadId :: {-# UNPACK #-} !ThreadId
    }

-- | The thread ID of the statsd sync thread. You can stop the sync by
-- killing this thread (i.e. by throwing it an asynchronous
-- exception.)
statsdThreadId :: Statsd -> ThreadId
statsdThreadId = threadId

data StatsdOptions = StatsdOptions
    { host          :: !String  -- ^ Server hostname or IP address
    , port          :: !Int     -- ^ Server port
    , flushInterval :: !Int     -- ^ Data push interval, in ms.
    , debug         :: !Bool    -- ^ Print debug output to stderr.
    }

defaultStatsdOptions :: StatsdOptions
defaultStatsdOptions = StatsdOptions
    { host          = "127.0.0.1"
    , port          = 8125
    , flushInterval = 1000
    , debug         = False
    }

forkStatsd :: StatsdOptions -> Metrics.Store -> IO Statsd
forkStatsd opts store = do
    addrInfos <- Socket.getAddrInfo Nothing (Just $ host opts)
                 (Just $ show $ port opts)
    socket <- case addrInfos of
        [] -> unsupportedAddressError
        (addrInfo:_) -> do
            socket <- Socket.socket (Socket.addrFamily addrInfo)
                      Socket.Datagram Socket.defaultProtocol
            Socket.connect socket (Socket.addrAddress addrInfo)
            return socket
    -- TODO: Make sure the socket gets closed?
    tid <- forkIO $ loop store emptySample socket opts
    return $ Statsd tid
  where
    unsupportedAddressError = ioError $ userError $
        "unsupported address: " ++ host opts
    emptySample = M.empty

loop :: Metrics.Store
     -> Metrics.Metrics  -- ^ Last sampled metrics
     -> Socket.Socket    -- ^ Connected socket
     -> StatsdOptions
     -> IO ()
loop store lastSample socket opts = do
    start <- time
    sample <- Metrics.sampleAll store
    let !diff = diffSamples lastSample sample
    flushSample diff socket opts
    end <- time
    threadDelay (flushInterval opts * 1000 - fromIntegral (end - start))
    loop store sample socket opts

-- | Microseconds since epoch.
time :: IO Int64
time = (round . (* 1000000.0) . toDouble) `fmap` getPOSIXTime
  where toDouble = realToFrac :: Real a => a -> Double

diffSamples :: Metrics.Metrics -> Metrics.Metrics -> Metrics.Metrics
diffSamples old new = diffMetrics old new

-- TODO: Combine different metrics somehow.
diffMetrics :: M.HashMap T.Text Metrics.Value -> M.HashMap T.Text Metrics.Value
            -> M.HashMap T.Text Metrics.Value
diffMetrics old new = M.foldlWithKey' combine M.empty new
  where
    combine m name val = case M.lookup name old of
        Just val'
            | val == val' -> m
        _                 -> M.insert name val m

flushSample :: Metrics.Metrics -> Socket.Socket -> StatsdOptions -> IO ()
flushSample sample socket opts = do
    forM_ (M.toList $ sample) $ \ (name, val) ->
        flushMetric name val
  where
    flushMetric name (Metrics.Counter n) = send "|c" name (show n)
    flushMetric name (Metrics.Gauge n)   = send "|g" name (show n)
    flushMetric _ _                      = return ()

    isDebug = debug opts
    send ty name val = do
        let msg = B8.concat
                  [ T.encodeUtf8 name
                  , ":"
                  , B8.pack val
                  , ty
                  ]
        when isDebug $ B8.hPutStrLn stderr $ B8.concat
            [ "DEBUG: "
            , msg
            ]
        Socket.sendAll socket msg
