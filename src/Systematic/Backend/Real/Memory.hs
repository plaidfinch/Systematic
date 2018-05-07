module Systematic.Backend.Real.Memory
  ( Memory
  , memory
  ) where

import Systematic.Language
import Systematic.Enumerator

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch
import Control.Monad.Fix

import Control.Concurrent as GHC hiding (ThreadId)
import Data.IORef

data AllocCounters
  = AllocCounters
      { refIds  :: Enumerator Int
      , varIds  :: Enumerator Int
      , chanIds :: Enumerator Int
      }

initializeAllocCounters :: MonadIO m => m AllocCounters
initializeAllocCounters =
  liftIO $ do
    r <- enumerateFrom 0
    v <- enumerateFrom 0
    c <- enumerateFrom 0
    return $ AllocCounters r v c

newtype Memory m a
  = Memory (ReaderT AllocCounters m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadFix
    , HasLog, HasTextLog, HasThreads, HasSockets )

instance MonadTrans Memory where
  lift = Memory . lift

memory :: MonadIO m => Memory m a -> m a
memory (Memory action) =
  runReaderT action =<< initializeAllocCounters

data RealRef a
  = RealRef
      { refIdentity :: Int
      , wrappedRef  :: IORef a }

instance RefInfo RealRef where
  refId = refIdentity

data RealVar a
  = RealVar
      { varIdentity :: Int
      , wrappedVar  :: MVar a }

instance VarInfo RealVar where
  varId = varIdentity

data RealChannel a
  = RealChan
      { chanIdentity :: Int
      , wrappedChan  :: Chan a }

instance ChannelInfo RealChannel where
  channelId = chanIdentity

instance MonadIO m => HasMemory (Memory m) where
  type Ref (Memory m) = RealRef
  newRef val = Memory $ do
    refIdentity <- liftIO . nextEnum =<< asks refIds
    wrappedRef  <- liftIO $ newIORef val
    return RealRef{refIdentity, wrappedRef}
  readRef RealRef{wrappedRef} =
    liftIO $ readIORef wrappedRef
  writeRef RealRef{wrappedRef} val =
    liftIO $ writeIORef wrappedRef val

  type Var (Memory m) = RealVar
  newEmptyVar = Memory $ do
    varIdentity <- liftIO . nextEnum =<< asks varIds
    wrappedVar  <- liftIO newEmptyMVar
    return RealVar{varIdentity, wrappedVar}
  newVar val = Memory $ do
    varIdentity <- liftIO . nextEnum =<< asks varIds
    wrappedVar  <- liftIO $ newMVar val
    return RealVar{varIdentity, wrappedVar}
  takeVar RealVar{wrappedVar} =
    liftIO $ takeMVar wrappedVar
  putVar RealVar{wrappedVar} val =
    liftIO $ putMVar wrappedVar val

  type Channel (Memory m) = RealChannel
  newChan = Memory $ do
    chanIdentity <- liftIO . nextEnum =<< asks chanIds
    wrappedChan  <- liftIO GHC.newChan
    return RealChan{chanIdentity, wrappedChan}
  readChan RealChan{wrappedChan} =
    liftIO $ GHC.readChan wrappedChan
  writeChan RealChan{wrappedChan} val =
    liftIO $ GHC.writeChan wrappedChan val
