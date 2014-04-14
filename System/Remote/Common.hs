{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Common
    (
      buildMany
    , buildAll
    , buildCombined
    , encodeMetrics
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

data MetricType =
      CounterType
    | GaugeType
    | LabelType

metricType :: MetricType -> T.Text
metricType CounterType = "c"
metricType GaugeType   = "g"
metricType LabelType   = "l"

-- | Encode metrics as nested JSON objects. Each "." in the metric
-- name introduces a new level of nesting. For example, the metrics
-- @[("foo.bar", 10), ("foo.baz", "label")]@ are encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": 10,
-- >     "baz": "label"
-- >   }
-- > }
--
encodeMetrics :: Metrics -> L.ByteString
encodeMetrics metrics =
    A.encode $
    buildOne (metricsCounters metrics) (metricType CounterType) $
    buildOne (metricsGauges metrics) (metricType GaugeType) $
    buildOne (metricsLabels metrics) (metricType LabelType) $ A.emptyObject
  where
    buildOne :: A.ToJSON a => M.HashMap T.Text a -> T.Text -> A.Value -> A.Value
    buildOne m ty o = M.foldlWithKey' (build ty) o m

    build :: A.ToJSON a => T.Text -> A.Value -> T.Text -> a -> A.Value
    build ty m name val = go ty m (T.splitOn "." name) val

    go :: A.ToJSON a => T.Text -> A.Value -> [T.Text] -> a -> A.Value
    go ty (A.Object m) [str] val      = A.Object $ M.insert str metric m
      where metric = A.object [("val", A.toJSON val), ("type", A.toJSON ty)]
    go ty (A.Object m) (str:rest) val = case M.lookup str m of
        Nothing -> A.Object $ M.insert str (go ty A.emptyObject rest val) m
        Just m' -> A.Object $ M.insert str (go ty m' rest val) m
    go _ v _ _                       = typeMismatch "Object" v

    {-# SPECIALIZE buildOne :: M.HashMap T.Text Int -> T.Text -> A.Value
                            -> A.Value #-}
    {-# SPECIALIZE buildOne :: M.HashMap T.Text T.Text -> T.Text -> A.Value
                            -> A.Value #-}
    {-# SPECIALIZE build :: T.Text -> A.Value -> T.Text -> Int -> A.Value #-}
    {-# SPECIALIZE build :: T.Text -> A.Value -> T.Text -> T.Text -> A.Value #-}
    {-# SPECIALIZE go :: T.Text -> A.Value -> [T.Text] -> Int -> A.Value #-}
    {-# SPECIALIZE go :: T.Text -> A.Value -> [T.Text] -> T.Text -> A.Value #-}

typeMismatch :: String   -- ^ The expected type
             -> A.Value  -- ^ The actual value encountered
             -> a
typeMismatch expected actual =
    error $ "when expecting a " ++ expected ++ ", encountered " ++ name ++
    " instead"
  where
    name = case actual of
        A.Object _ -> "Object"
        A.Array _  -> "Array"
        A.String _ -> "String"
        A.Number _ -> "Number"
        A.Bool _   -> "Boolean"
        A.Null     -> "Null"
