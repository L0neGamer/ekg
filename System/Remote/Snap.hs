{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Snap
    ( startServer
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Network.Socket (NameInfoFlag(NI_NUMERICHOST), addrAddress, getAddrInfo,
                       getNameInfo)
import Paths_ekg (getDataDir)
import Prelude hiding (read)
import Snap.Core (MonadSnap, Request, Snap, finishWith, getHeaders, getRequest,
                  getResponse, method, Method(GET), modifyResponse, pass,
                  rqPathInfo, setContentType, setResponseStatus,
                  writeLBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Config
import Snap.Util.FileServe (serveDirectory)
import System.FilePath ((</>))

import System.Metrics
import System.Remote.Json

------------------------------------------------------------------------

startServer :: Store
            -> S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
            -> Int           -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer store host port = do
    let conf = Config.setVerbose False $
               Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               Config.defaultConfig
    httpServe conf (monitor store)

-- | A handler that can be installed into an existing Snap application.
monitor :: Store -> Snap ()
monitor store = do
    dataDir <- liftIO getDataDir
    (jsonHandler $ serve store)
        <|> serveDirectory (dataDir </> "assets")
  where
    jsonHandler = wrapHandler "application/json"
    wrapHandler fmt handler = method GET $ format fmt $ handler

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

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
serve :: MonadSnap m => Store -> m ()
serve store = do
    req <- getRequest
    modifyResponse $ setContentType "application/json"
    if S.null (rqPathInfo req)
        then serveAll
        else serveOne (rqPathInfo req)
  where
    serveAll = do
        metrics <- liftIO $ sampleAll store
        writeLBS $ encodeAll metrics
    serveOne pathInfo = do
        let segments  = S8.split '/' pathInfo
            nameBytes = S8.intercalate "." segments
        case T.decodeUtf8' nameBytes of
            Left _ -> do
                modifyResponse $ setResponseStatus 400 "Bad Request"
                r <- getResponse
                finishWith r
            Right name -> do
                metrics <- liftIO $ sampleAll store
                case M.lookup name metrics of
                    Nothing -> pass
                    Just metric -> writeLBS $ encodeOne metric

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
