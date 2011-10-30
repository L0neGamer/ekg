{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module System.Remote.Monitoring where

import Control.Monad.IO.Class
import qualified Data.Aeson.Encode as A
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Enumerator
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Word
import GHC.Stats
import Network.HTTP.Types
import Network.Wai

instance A.ToJSON GCStats where
    toJSON (GCStats {..}) = A.object
        [ "bytes_allocated"          .= bytesAllocated
        , "num_gcs"                  .= numGcs
        , "max_bytes_used"           .= maxBytesUsed
        , "num_bytes_usage_samples"  .= numByteUsageSamples
        , "cumulative_butes_used"    .= cumulativeBytesUsed
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

getStatsAsJson :: IO L.ByteString
getStatsAsJson = (A.encode . A.toJSON) `fmap` getGCStats

genericHandler :: Application
genericHandler = toIteratee
               . fromMaybe response406
               . firstMatchingHandler
               . contentTypes
  where
    firstMatchingHandler :: [S.ByteString] -> Maybe (IO Response)
    firstMatchingHandler = getFirst . mconcat
                         . List.map (First . flip lookup handlers)

    contentTypes :: Request -> [S.ByteString]
    contentTypes = maybe [] parseHttpAccept . lookup "Accept" . requestHeaders

toIteratee :: IO Response -> Iteratee S.ByteString IO Response
toIteratee m = liftIO m

jsonHandler :: IO Response
jsonHandler = do
    bs <- getStatsAsJson
    return $! responseLBS statusOK [("Content-Type", "application/json")] bs

indexHandler :: IO Response
indexHandler = return $ ResponseFile statusOK [("Content-Type", "text/html")]
               "public/index.html" Nothing

handlers :: [(S.ByteString, IO Response)]
handlers =
    [ ("text/json", jsonHandler)
    , ("application/json", jsonHandler)
    , ("text/html", indexHandler)
    , ("*/*", jsonHandler)
    ]

ok = undefined

response406 :: IO Response
response406 = return $ responseLBS status404 [] L.empty  -- should be 406

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
