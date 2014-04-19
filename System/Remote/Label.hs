{-# LANGUAGE BangPatterns #-}
-- | This module defines a type for mutable, string-valued labels.
-- Labels are variable values and can be used to track e.g. the
-- command line arguments or other free-form values. All operations on
-- labels are thread-safe.
module System.Remote.Label
    (
      Label
    , new
    , read
    , set
    , modify
    ) where

import Data.IORef (IORef, atomicModifyIORef, newIORef, readIORef)
import qualified Data.Text as T
import Prelude hiding (read)

-- | A mutable, text-valued label.
newtype Label = C { unC :: IORef T.Text }

-- | Create a new empty label.
new :: IO Label
new = C `fmap` newIORef T.empty

-- | Get the current value of the label.
read :: Label -> IO T.Text
read = readIORef . unC

-- | Set the label to the given value.
set :: Label -> T.Text -> IO ()
set (C ref) !i = atomicModifyIORef ref $ \ _ -> (i, ())

-- | Set the label to the result of applying the given function to the
-- value.
modify :: (T.Text -> T.Text) -> Label -> IO ()
modify f (C ref) = do
    !_ <- atomicModifyIORef ref $ \ i -> let i' = f i in (i', i')
    return ()
