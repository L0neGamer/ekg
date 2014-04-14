{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Common
    (
      buildMany
    , buildAll
    , buildCombined
    ) where

import qualified Data.Aeson.Encode as A
import Data.Aeson.Types ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Prelude hiding (read)

import System.Metrics

------------------------------------------------------------------------
-- * JSON serialization

-- | A list of string keys and JSON-encodable values.  Used to render
-- a list of key-value pairs as a JSON object.
newtype Assocs = Assocs [(T.Text, Json)]

instance A.ToJSON Assocs where
    toJSON (Assocs xs) = A.object $ map (uncurry (.=)) xs

-- | A group of either counters or gauges.
data Group = Group
     ![(T.Text, Json)]
    {-# UNPACK #-} !Double  -- Milliseconds since epoch

instance A.ToJSON Group where
    toJSON (Group xs t) =
        A.object $ ("server_timestamp_millis" .= t) : map (uncurry (.=)) xs

------------------------------------------------------------------------

-- Existential wrapper used for OO-style polymorphism.
data Json = forall a. A.ToJSON a => Json a

instance A.ToJSON Json where
    toJSON (Json x) = A.toJSON x

------------------------------------------------------------------------

-- TODO: Move the sampling into 'buildMany'.

-- | Serve a collection of counters or gauges, as a JSON object.
buildMany :: A.ToJSON t => (M.HashMap T.Text t) -> IO L.ByteString
buildMany metrics = do
    return $! A.encode $ A.toJSON $ Assocs $ map (mapSnd Json) $
        M.toList metrics
{-# INLINABLE buildMany #-}

-- | Serve all counter, gauges and labels, built-in or not, as a
-- nested JSON object.
buildAll :: Store -> IO L.ByteString
buildAll = buildCombined
-- We're keeping this function from b/w compat but it now behaves
-- as 'buildCombined'.

newtype MetricValue = MetricValue Metric

instance A.ToJSON MetricValue where
    toJSON (MetricValue (Counter n)) = A.toJSON n
    toJSON (MetricValue (Gauge n))   = A.toJSON n
    toJSON (MetricValue (Label l))   = A.toJSON l

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

buildCombined :: Store -> IO L.ByteString
buildCombined store = do
    metrics <- sampleCombined store
    return $ A.encode $ A.toJSON $ Assocs $ map (mapSnd (Json . MetricValue)) $
        M.toList metrics

