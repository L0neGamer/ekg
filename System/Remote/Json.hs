{-# LANGUAGE OverloadedStrings #-}

module System.Remote.Json
    (
      encodeAll
    , encodeOne
    ) where

import qualified Data.Aeson.Encode as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Prelude hiding (read)

import System.Metrics

------------------------------------------------------------------------
-- * JSON serialization

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
encodeAll :: Metrics -> L.ByteString
encodeAll metrics =
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
    go _ v _ _                        = typeMismatch "Object" v

    {-# SPECIALIZE buildOne :: M.HashMap T.Text Int -> T.Text -> A.Value
                            -> A.Value #-}
    {-# SPECIALIZE buildOne :: M.HashMap T.Text T.Text -> T.Text -> A.Value
                            -> A.Value #-}
    {-# SPECIALIZE build :: T.Text -> A.Value -> T.Text -> Int -> A.Value #-}
    {-# SPECIALIZE build :: T.Text -> A.Value -> T.Text -> T.Text -> A.Value #-}
    {-# SPECIALIZE go :: T.Text -> A.Value -> [T.Text] -> Int -> A.Value #-}
    {-# SPECIALIZE go :: T.Text -> A.Value -> [T.Text] -> T.Text -> A.Value #-}

encodeOne :: Metric -> L.ByteString
encodeOne (Counter n) = encodeMetric n CounterType
encodeOne (Gauge n)   = encodeMetric n GaugeType
encodeOne (Label l)   = encodeMetric l LabelType

encodeMetric :: A.ToJSON a => a -> MetricType -> L.ByteString
encodeMetric val ty = A.encode $ A.object [
    ("val", A.toJSON val), ("type", A.toJSON (metricType ty))]
{-# SPECIALIZE encodeMetric :: Int -> MetricType -> L.ByteString #-}
{-# SPECIALIZE encodeMetric :: T.Text -> MetricType -> L.ByteString #-}

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
