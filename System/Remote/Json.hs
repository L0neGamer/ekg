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
    A.encode $ buildOne metrics $ A.emptyObject
  where
    buildOne :: M.HashMap T.Text Value -> A.Value -> A.Value
    buildOne m o = M.foldlWithKey' build o m

    build :: A.Value -> T.Text -> Value -> A.Value
    build m name val = go m (T.splitOn "." name) val

    go :: A.Value -> [T.Text] -> Value -> A.Value
    go (A.Object m) [str] val      = A.Object $ M.insert str metric m
      where metric = buildOneM val
    go (A.Object m) (str:rest) val = case M.lookup str m of
        Nothing -> A.Object $ M.insert str (go A.emptyObject rest val) m
        Just m' -> A.Object $ M.insert str (go m' rest val) m
    go v _ _                        = typeMismatch "Object" v

buildOneM :: Value -> A.Value
buildOneM (Counter n) = goOne n CounterType
buildOneM (Gauge n)   = goOne n GaugeType
buildOneM (Label l)   = goOne l LabelType

goOne :: A.ToJSON a => a -> MetricType -> A.Value
goOne val ty = A.object [
    ("val", A.toJSON val), ("type", A.toJSON (metricType ty))]

encodeOne :: Value -> L.ByteString
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
