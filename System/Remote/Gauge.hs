-- | This module defines a type for mutable, integer-valued gauges.
-- Gauges are variable values and can be used to track e.g. the
-- current number of concurrent connections. All operations on gauges
-- are thread-safe.
module System.Remote.Gauge
    (
      Gauge.Gauge
    , Gauge.inc
    , Gauge.dec
    , Gauge.add
    , Gauge.subtract
    , Gauge.set
    ) where

import qualified System.Metrics.Gauge as Gauge
