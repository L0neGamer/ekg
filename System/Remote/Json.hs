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
import Data.Int (Int64)
import qualified Data.Text as T
import Prelude hiding (read)

import System.Metrics
import qualified System.Metrics.Distribution as Distribution

------------------------------------------------------------------------
-- * JSON serialization

data MetricType =
      CounterType
    | GaugeType
    | LabelType
    | DistributionType

metricType :: MetricType -> T.Text
metricType CounterType      = "c"
metricType GaugeType        = "g"
metricType LabelType        = "l"
metricType DistributionType = "d"

-- | Encode metrics as nested JSON objects. Each "." in the metric
-- name introduces a new level of nesting. For example, the metrics
-- @[("foo.bar", 10), ("foo.baz", "label")]@ are encoded as
--
-- > {
-- >   "foo": {
-- >     "bar": {
-- >       "type:", "c",
-- >       "val": 10
-- >     },
-- >     "baz": {
-- >       "type": "l",
-- >       "val": "label"
-- >     }
-- >   }
-- > }
--
encodeAll :: Sample -> L.ByteString
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
buildOneM (Counter n)      = goOne n CounterType
buildOneM (Gauge n)        = goOne n GaugeType
buildOneM (Label l)        = goOne l LabelType
buildOneM (Distribution l) = goDistribution l

goDistribution :: Distribution.Stats -> A.Value
goDistribution stats = A.object [
    ("mean", A.toJSON $! Distribution.mean stats),
    ("variance", A.toJSON $! Distribution.variance stats),
    ("count", A.toJSON $! Distribution.count stats),
    ("sum", A.toJSON $! Distribution.sum stats),
    ("min", A.toJSON $! Distribution.min stats),
    ("max", A.toJSON $! Distribution.max stats),
    ("type", A.toJSON (metricType DistributionType))]

goOne :: A.ToJSON a => a -> MetricType -> A.Value
goOne val ty = A.object [
    ("val", A.toJSON val), ("type", A.toJSON (metricType ty))]
{-# SPECIALIZE goOne :: Int64 -> MetricType -> A.Value #-}
{-# SPECIALIZE goOne :: T.Text -> MetricType -> A.Value #-}

encodeOne :: Value -> L.ByteString
encodeOne = A.encode . buildOneM

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
