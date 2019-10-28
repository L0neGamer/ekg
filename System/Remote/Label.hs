{-# OPTIONS_HADDOCK not-home #-}

-- | This module defines a type for mutable, string-valued labels.
-- Labels are variable values and can be used to track e.g. the
-- command line arguments or other free-form values. All operations on
-- labels are thread-safe.
--
-- N.B. This module exists to maintain backwards compatibility with
-- older versions of this library. New code should use the
-- @System.Metrics.Label@ module from the ekg-core package instead.
module System.Remote.Label
    (
      Label.Label
    , Label.set
    , Label.modify
    ) where

import qualified System.Metrics.Label as Label
