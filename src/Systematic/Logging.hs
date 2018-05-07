module Systematic.Logging
  ( Logging, loggingWith
  ) where

import Systematic.Language

import Data.Typeable
import Data.Coerce
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader


newtype Logging m a
  = Logging (ReaderT (Logger m) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

newtype Logger m
  = Logger (forall a. (Typeable a, Show a) => a -> m ())

withLogger :: (Logger m -> m a) -> Logging m a
withLogger = coerce

loggingWith
  :: (forall x. (Typeable x, Show x) => x -> m ()) -> Logging m a -> m a
loggingWith logger action =
  coerce action (Logger logger)

instance Monad m => HasLog (Logging m) where
  log message = Logging . ReaderT $
    \(Logger logger) -> logger message


-- Boilerplate

instance MonadTrans Logging where
  lift = Logging . lift

instance HasThreads m => HasThreads (Logging m) where
  type ThreadId (Logging m) = ThreadId m
  fork process =
    withLogger $ \(Logger logger) ->
      fork $ loggingWith logger process
  kill = lift . kill

instance HasMemory m => HasMemory (Logging m) where
  type Ref (Logging m) = Ref m
  newRef    = lift .  newRef
  readRef   = lift .  readRef
  writeRef  = lift .: writeRef

  type Var (Logging m) = Var m
  newVar      = lift .  newVar
  newEmptyVar = lift    newEmptyVar
  takeVar     = lift .  takeVar
  putVar      = lift .: putVar

  type Channel (Logging m) = Channel m
  newChan   = lift    newChan
  readChan  = lift .  readChan
  writeChan = lift .: writeChan

instance HasSockets m => HasSockets (Logging m) where
  type Socket (Logging m) = Socket m
  connect      = (lift .:) .: connect
  listen       = lift .: listen
  accept       = lift .  accept
  send         = lift .: send
  receive      = lift .  receive
  close        = lift .  close
