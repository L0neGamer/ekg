{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Snap
    ( startServer
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Exception (throwIO)
import Control.Monad (join, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Network.Socket (NameInfoFlag(NI_NUMERICHOST), addrAddress, getAddrInfo,
                       getNameInfo)
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

import System.Metrics
import System.Remote.Common

------------------------------------------------------------------------

-- | Convert a host name (e.g. \"localhost\" or \"127.0.0.1\") to a
-- numeric host address (e.g. \"127.0.0.1\").
getNumericHostAddress :: S.ByteString -> IO S.ByteString
getNumericHostAddress host = do
    ais <- getAddrInfo Nothing (Just (S8.unpack host)) Nothing
    case ais of
        [] -> unsupportedAddressError
        (ai:_) -> do
            ni <- getNameInfo [NI_NUMERICHOST] True False (addrAddress ai)
            case ni of
                (Just numericHost, _) -> return $! S8.pack numericHost
                _ -> unsupportedAddressError
  where
    unsupportedAddressError = throwIO $
        userError $ "unsupported address: " ++ S8.unpack host

startServer :: MetricStore
            -> S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
            -> Int           -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer store host port = do
    -- Snap doesn't allow for non-numeric host names in
    -- 'Snap.setBind'. We work around that limitation by converting a
    -- possible non-numeric host name to a numeric address.
    numericHost <- getNumericHostAddress host
    let conf = Config.setVerbose False $
               Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               Config.setHostname host $
               Config.setBind numericHost $
               Config.defaultConfig
    httpServe conf (monitor store)

-- | The routes of the ekg monitor. They do not include the routes for its
-- assets.
monitorRoutes :: MonadSnap m
              => MetricStore
              -> [(S8.ByteString, m ())]
monitorRoutes store =
    [ ("",               jsonHandler $ serveAll store)
    , ("combined",       jsonHandler $ serveCombined store)
    , ("counters",       jsonHandler $ serveMany (sampleCounters store))
    , ("counters/:name", textHandler $ serveOne (flip sampleCounter store))
    , ("gauges",         jsonHandler $ serveMany (sampleGauges store))
    , ("gauges/:name",   textHandler $ serveOne (flip sampleGauge store))
    , ("labels",         jsonHandler $ serveMany (sampleLabels store))
    , ("labels/:name",   textHandler $ serveOne (flip sampleLabel store))
    ]
  where
    jsonHandler = wrapHandler "application/json"
    textHandler = wrapHandler "text/plain"
    wrapHandler fmt handler = method GET $ format fmt $ do
        req <- getRequest
        -- We only want to handle completely matched paths.
        if S.null (rqPathInfo req) then handler else pass

-- | A handler that can be installed into an existing Snap application.
monitor :: MetricStore -> Snap ()
monitor store = do
    dataDir <- liftIO getDataDir
    route (monitorRoutes store)
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

-- | Serve a collection of counters or gauges, as a JSON object.
serveMany :: (A.ToJSON t, MonadSnap m)
          => (IO (M.HashMap T.Text t)) -> m ()
serveMany sample = do
    metrics <- liftIO sample
    modifyResponse $ setContentType "application/json"
    bs <- liftIO $ buildMany metrics
    writeLBS bs
{-# INLINABLE serveMany #-}

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
serveAll :: MonadSnap m => MetricStore -> m ()
serveAll store = do
    req <- getRequest
    -- Workaround: Snap still matches requests to /foo to this handler
    -- if the Accept header is "application/json", even though such
    -- requests ought to go to the 'serveOne' handler.
    unless (S.null $ rqPathInfo req) pass
    modifyResponse $ setContentType "application/json"
    bs <- liftIO $ buildAll store
    writeLBS bs

-- | Serve all counters and gauges, built-in or not, as a flattened
-- JSON object.
serveCombined :: MonadSnap m => MetricStore -> m ()
serveCombined store = do
    modifyResponse $ setContentType "application/json"
    bs <- liftIO $ buildCombined store
    writeLBS bs

-- | Serve a single counter, as plain text.
serveOne :: (Show t, MonadSnap m)
         => (T.Text -> IO (Maybe t)) -> m ()
serveOne sample = do
    modifyResponse $ setContentType "text/plain"
    req <- getRequest
    let mname = T.decodeUtf8 <$> join
                (listToMaybe <$> Map.lookup "name" (rqParams req))
    case mname of
        Nothing -> pass
        Just name -> do
            mmetric <- liftIO $ sample name
            case mmetric of
                Just val -> writeBS $ S8.pack $ show val
                Nothing  -> do
                    modifyResponse $ setResponseStatus 404 "Not Found"
                    r <- getResponse
                    finishWith r
{-# INLINABLE serveOne #-}

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
