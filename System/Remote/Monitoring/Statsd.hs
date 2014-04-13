{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module System.Remote.Monitoring.Statsd
    (
      forkStatsd
    , StatsdOptions
    , defaultStatsdOptions
    , Statsd
    , statsdThreadId
    ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (forever, forM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Encoding as T
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket
import qualified System.Metrics as Metrics

data Statsd = Statsd
    { threadId :: {-# UNPACK #-} !ThreadId
    }

statsdThreadId :: Statsd -> ThreadId
statsdThreadId = threadId

data StatsdOptions = StatsdOptions
    { host         :: !String  -- ^ Server hostname or IP address
    , port         :: !Int     -- ^ Server port
    , syncInterval :: !Int     -- ^ Data push interval, in ms.
    }

defaultStatsdOptions :: StatsdOptions
defaultStatsdOptions = StatsdOptions
    { host         = "localhost"
    , port         = 3000
    , syncInterval = 10000
    }

forkStatsd :: StatsdOptions -> Metrics.MetricStore -> IO Statsd
forkStatsd opts store = do
    addrInfos <- Socket.getAddrInfo Nothing (Just $ host opts) Nothing
    socket <- case addrInfos of
        [] -> unsupportedAddressError
        (addrInfo:_) -> do
            socket <- Socket.socket (Socket.addrFamily addrInfo)
                      Socket.Datagram Socket.defaultProtocol
            Socket.connect socket (Socket.addrAddress addrInfo)
            return socket
    -- TODO: Make sure the socket gets closed.
    lastSample <- Metrics.sampleAll store
    tid <- forkIO $ do
        flushSample lastSample socket
        flush store lastSample socket
    return $ Statsd tid
  where
    unsupportedAddressError = ioError $ userError $
        "unsupported address: " ++ host opts

flush :: Metrics.MetricStore -> Metrics.Metrics -> Socket.Socket -> IO ()
flush store lastSample socket = forever $ do
    return ()

diffSamples :: Metrics.Metrics -> Metrics.Metrics -> Metrics.Metrics
diffSamples s1 s2 = s1  -- TODO

flushSample :: Metrics.Metrics -> Socket.Socket -> IO ()
flushSample Metrics.Metrics{..} socket = do
    forM_ (M.toList $ metricsCounters) $ \ (name, val) ->
        Socket.sendAll socket $ B8.concat
        [ T.encodeUtf8 name
        , ":"
        , B8.pack $ show val
        , "|c"
        ]
