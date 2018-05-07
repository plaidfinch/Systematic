module Systematic.Backend.Real.Base where

import Systematic.Language
import Systematic.Enumerator

import Control.Concurrent hiding (ThreadId)
import qualified Control.Concurrent as Conc
import Data.Functor

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Fix


newtype Base a
  = Base (EnumeratorT Int IO a)
  deriving newtype (Functor, Applicative, Monad,
                    MonadIO, MonadThrow, MonadCatch, MonadFix)

run :: Base a -> IO a
run (Base process) =
  runEnumeratorT 0 process

data RealThreadId
  = RealThreadId Int Conc.ThreadId

instance ThreadInfo RealThreadId where
  threadId (RealThreadId i _) = i

instance HasThreads Base where
  type ThreadId Base = RealThreadId
  fork (Base process) = Base $ do
    e <- enumerator
    i <- next
    tid <- liftIO . forkIO . void $
             withEnumerator e process
    return (RealThreadId i tid)
  kill (RealThreadId _ tid) =
    liftIO (killThread tid)

instance HasTextLog Base where
  appendLogString = liftIO . putStrLn
