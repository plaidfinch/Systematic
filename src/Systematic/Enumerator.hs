module Systematic.Enumerator
  ( Enumerator
  , enumerateFrom
  , next
  ) where

import Data.IORef

newtype Enumerator a
  = Enumerator (IORef a)

enumerateFrom :: a -> IO (Enumerator a)
enumerateFrom a = do
  ref <- newIORef a
  return (Enumerator ref)

next :: Enum a => Enumerator a -> IO a
next (Enumerator ref) = do
  atomicModifyIORef' ref (\a -> (succ a, a))
