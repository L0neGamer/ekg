{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Snap
    ( startServer
    ) where
    
import Control.Applicative ((<$>), (<|>))
import Control.Monad (join, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as M
import Data.IORef (IORef)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

import System.Remote.Common

------------------------------------------------------------------------

startServer :: IORef Counters -> IORef Gauges -> IORef Labels
            -> S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
            -> Int           -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer counters gauges labels host port =
    httpServe conf (monitor counters gauges labels)
  where conf = Config.setVerbose False $
               Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               Config.setHostname host $
               Config.setBind host $
               Config.defaultConfig


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

-- | Serve a collection of counters or gauges, as a JSON object.
serveMany :: (Ref r t, A.ToJSON t) => IORef (M.HashMap T.Text r) -> Snap ()
serveMany mapRef = do
    modifyResponse $ setContentType "application/json"
    bs <- liftIO $ buildMany mapRef
    writeLBS bs
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
    bs <- liftIO $ buildAll counters gauges labels
    writeLBS bs

-- | Serve all counters and gauges, built-in or not, as a flattened
-- JSON object.
serveCombined :: IORef Counters -> IORef Gauges -> IORef Labels -> Snap ()
serveCombined counters gauges labels = do
    modifyResponse $ setContentType "application/json"
    bs <- liftIO $ buildCombined counters gauges labels
    writeLBS bs

-- | Serve a single counter, as plain text.
serveOne :: (Ref r t, Show t) => IORef (M.HashMap T.Text r) -> Snap ()
serveOne refs = do
    modifyResponse $ setContentType "text/plain"
    req <- getRequest
    let mname = T.decodeUtf8 <$> join
                (listToMaybe <$> Map.lookup "name" (rqParams req))
    case mname of
        Nothing -> pass
        Just name -> do
            mbs <- liftIO $ buildOne refs name
            case mbs of
                Just bs -> writeBS bs
                Nothing -> do
                    modifyResponse $ setResponseStatus 404 "Not Found"
                    r <- getResponse
                    finishWith r
{-# INLINABLE serveOne #-}
