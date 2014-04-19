{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, string-valued labels.
-- Labels are variable values and can be used to track e.g. the
-- command line arguments or other free-form values. All operations on
-- labels are thread-safe.
module System.Remote.Label
    (
      Label.Label
    , Label.set
    , Label.modify
    ) where

import qualified System.Metrics.Label as Label
