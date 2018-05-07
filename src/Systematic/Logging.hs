module Systematic.Logging
  ( Logging, loggingWith
  ) where

import Systematic.Language

import Data.Typeable
import Data.Coerce
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch
import Control.Monad.Fix

newtype Logging m a
  = Logging (ReaderT (Logger m) m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadFix
    , HasTextLog, HasThreads, HasSockets, HasMemory )

instance MonadTrans Logging where
  lift = Logging . lift

newtype Logger m
  = Logger (forall a. (Typeable a, Show a) => a -> m ())

loggingWith
  :: (forall x. (Typeable x, Show x) => x -> m ()) -> Logging m a -> m a
loggingWith logger action =
  coerce action (Logger logger)

instance Monad m => HasLog (Logging m) where
  log message = Logging . ReaderT $
    \(Logger logger) -> logger message
