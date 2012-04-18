{-# OPTIONS_HADDOCK not-home #-}
module System.Remote.Label.Internal
    (
      Label(..)
    , new
    , read
    ) where

import Data.IORef (IORef, newIORef, readIORef)
import qualified Data.Text as T
import Prelude hiding (read)

-- | A mutable, text-valued label.
newtype Label = C { unC :: IORef T.Text }

-- | Create a new empty label.
new :: IO Label
new = C `fmap` newIORef T.empty

read :: Label -> IO T.Text
read = readIORef . unC
