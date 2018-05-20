module Systematic.Backend.Real.Base (Base, base) where

import Systematic.Language

import Control.Concurrent hiding (ThreadId)
import qualified Control.Concurrent as GHC

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Fix

import Data.Coerce


newtype Base a
  = Base (IO a)
  deriving newtype
    ( Functor, Applicative, Monad,
      MonadIO, MonadThrow, MonadCatch, MonadFix )

base :: Base a -> IO a
base = coerce

instance HasThreads Base where
  type ThreadId Base = GHC.ThreadId
  fork (Base process) = Base $ do
    tid <- liftIO . forkIO $ process
    return tid
  kill tid =
    liftIO (killThread tid)
  yield = return ()

instance HasTextLog Base where
  appendLogString = liftIO . putStrLn
