-- | This module defines a type for tracking statistics about a series
-- of events. An event could be handling of a request and the value
-- associated with the event -- the value you'd pass to 'add' -- could
-- be the amount of time spent serving that request. All operations
-- are thread safe.
module System.Remote.Distribution
    ( Distribution.Distribution
    , Distribution.new
    , Distribution.add
    , Distribution.addN
    ) where

import qualified System.Metrics.Distribution as Distribution
