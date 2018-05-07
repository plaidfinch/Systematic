module Systematic.Enumerator
  ( Enumerator
  , enumerateFrom
  , peekEnum
  , nextEnum
  , EnumeratorT
  , runEnumeratorT
  , peek
  , next
  , enumerator
  , withEnumerator
  ) where

import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.Catch

newtype Enumerator a
  = Enumerator (IORef a)

enumerateFrom :: a -> IO (Enumerator a)
enumerateFrom a = do
  ref <- newIORef a
  return (Enumerator ref)

peekEnum :: Enumerator a -> IO a
peekEnum (Enumerator ref) =
  readIORef ref

nextEnum :: Enum a => Enumerator a -> IO a
nextEnum (Enumerator ref) =
  atomicModifyIORef' ref (\a -> (succ a, a))

newtype EnumeratorT e m a
  = EnumeratorT (ReaderT (Enumerator e) m a)
  deriving newtype (Functor, Applicative, Monad,
                    MonadIO, MonadThrow, MonadCatch)

instance MonadTrans (EnumeratorT e) where
  lift = EnumeratorT . ReaderT . const

runEnumeratorT :: MonadIO m => e -> EnumeratorT e m a -> m a
runEnumeratorT e act =
  flip withEnumerator act =<< liftIO (enumerateFrom e)

peek :: MonadIO m => EnumeratorT e m e
peek = EnumeratorT $
  liftIO . peekEnum =<< ask

next :: (MonadIO m, Enum e) => EnumeratorT e m e
next = EnumeratorT $
  liftIO . nextEnum =<< ask

enumerator :: Monad m => EnumeratorT e m (Enumerator e)
enumerator = EnumeratorT ask

withEnumerator :: Enumerator e -> EnumeratorT e m a -> m a
withEnumerator this (EnumeratorT act) =
  runReaderT act this
