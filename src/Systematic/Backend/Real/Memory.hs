module Systematic.Backend.Real.Memory
  ( Memory
  , memory
  ) where

import Systematic.Language

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Control.Monad.Fix

import Control.Concurrent as GHC hiding (ThreadId)
import Data.IORef


newtype Memory m a
  = Memory (m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadFix
    , HasLog, HasTextLog, HasThreads, HasSockets )

instance MonadTrans Memory where
  lift = Memory

memory :: Memory m a -> m a
memory (Memory action) = action

instance MonadIO m => HasMemory (Memory m) where
  type Ref (Memory m) = IORef
  newRef val       = liftIO $ newIORef val
  readRef ref      = liftIO $ readIORef ref
  writeRef ref val = liftIO $ writeIORef ref val
  modifyRef ref f  = liftIO $ atomicModifyIORef ref f

  type Var (Memory m) = MVar
  newEmptyVar       = liftIO newEmptyMVar
  newVar val        = liftIO $ newMVar val
  tryTakeVar var    = liftIO $ tryTakeMVar var
  tryPutVar var val = liftIO $ tryPutMVar var val

  type Channel (Memory m) = Chan
  newChan            = liftIO GHC.newChan
  readChan chan      = liftIO $ GHC.readChan chan
  writeChan chan val = liftIO $ GHC.writeChan chan val

instance MonadIO m => HasBlockingMemory (Memory m) where
  takeVar var    = liftIO $ takeMVar var
  readVar var    = liftIO $ readMVar var
  putVar var val = liftIO $ putMVar var val
