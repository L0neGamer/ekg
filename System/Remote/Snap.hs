{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Remote.Snap
    ( startServer
    ) where

import Control.Applicative ((<$>), (<|>))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (fromMaybe)
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Text.Encoding as T
import Data.Word (Word8)
import Network.Socket (NameInfoFlag(NI_NUMERICHOST), addrAddress, getAddrInfo,
                       getNameInfo)
import Prelude hiding (read)
import Snap.Core (MonadSnap, Request, Snap, finishWith, getHeader, getRequest,
                  getResponse, method, Method(GET), modifyResponse, pass,
                  rqURI, rqPathInfo,
                  setContentType, setContentLength, setResponseStatus, setResponseCode,
                  writeBS, writeLBS)
import Snap.Http.Server (httpServe)
import qualified Snap.Http.Server.Config as Config
import Snap.Util.FileServe (defaultMimeTypes)
import System.FilePath (takeExtension)

import System.Metrics
import System.Remote.Json
import Data.FileEmbed (embedDir)


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

startServer :: Store
            -> Maybe S.ByteString  -- ^ Host to listen on (e.g. \"localhost\")
            -> Int           -- ^ Port to listen on (e.g. 8000)
            -> IO ()
startServer store m_host port = do
    -- Snap doesn't allow for non-numeric host names in
    -- 'Snap.setBind'. We work around that limitation by converting a
    -- possible non-numeric host name to a numeric address.
    setBind <- case m_host of
        Just host -> do
            numericHost <- getNumericHostAddress host
            return $ Config.setHostname host . Config.setBind numericHost
        Nothing -> return id
    let conf = Config.setVerbose False $
               Config.setErrorLog Config.ConfigNoLog $
               Config.setAccessLog Config.ConfigNoLog $
               Config.setPort port $
               setBind $
               Config.defaultConfig
    httpServe conf (monitor store)

-- | A handler that can be installed into an existing Snap application.
monitor :: Store -> Snap ()
monitor store = do
    (jsonHandler $ serve store)
        <|>
        serveAssets
  where
    jsonHandler = wrapHandler "application/json"
    wrapHandler fmt handler = method GET $ format fmt $ handler

-- | The Accept header of the request.
acceptHeader :: Request -> Maybe S.ByteString
acceptHeader req = getHeader "Accept" req

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

-- | Serve the embedded assets.
serveAssets :: MonadSnap m => m ()
serveAssets = serveEmbeddedFiles $(embedDir "assets")

-- | Serve a list of files under the given filepaths while selecting the MIME
--type using the 'defaultMimeMap'.
serveEmbeddedFiles :: MonadSnap m => [(FilePath, S8.ByteString)] -> m ()
serveEmbeddedFiles files = do
      req <- getRequest
      fromMaybe pass $ M.lookup (rqURI req) table
    where
      table         = M.fromList $ do
          (path, content) <- files
          let err  = error $ "Failed to determine MIME type of '" ++ path ++ "'"
              mime = fromMaybe err $
                         M.lookup (takeExtension path) defaultMimeTypes
              response = do
                modifyResponse $
                  setContentType mime .
                  setContentLength (fromIntegral $ S8.length content) .
                  setResponseCode 200
                writeBS content
          path' <- 
            map S8.pack $
            if path == "index.html" || path == "index.htm"
              then ["/", "/index.html", "/index.htm"]
              else ["/" ++ path]
          return (path', response)
