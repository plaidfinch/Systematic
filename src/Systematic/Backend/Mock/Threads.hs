{-# LANGUAGE RecordWildCards #-}

module Systematic.Backend.Mock.Threads where

import Systematic.Language

import Control.Monad
import Control.Monad.Trans.Class

import Data.Coerce


-- TODO: Implement thread ids, killing threads?
-- or, remove thread ids and killing from the API?

data Scheduler a where
  Scheduler
    :: { waiting :: q
       , sleep   :: a -> q -> q
       , next    :: q -> Maybe (a, q)
       } -> Scheduler a

-- This comes from CIS 552 at UPenn
-- <https://www.seas.upenn.edu/~cis552/current/lectures/soln/TransC.html>

data Action m
  = Lift (m (Action m))
  | Fork (Action m) (Action m)
  | Stop

newtype Threads m a
  = Threads ((a -> Action m) -> Action m)

threads :: Monad m => (forall x. Scheduler x) -> Threads m a -> m ()
threads Scheduler{sleep, next, waiting = empty} m =
  schedule Scheduler{waiting = sleep (runThreads m (const Stop)) empty, ..}

schedule :: Monad m => Scheduler (Action m) -> m ()
schedule Scheduler{sleep, next, waiting} =
  case next waiting of
    Nothing -> return ()
    Just (action, rest) ->
      case action of
        Stop ->
          schedule
            Scheduler{waiting = rest, ..}
        Lift atom -> do
          result <- atom
          schedule
            Scheduler{waiting = sleep result rest, ..}
        Fork action1 action2 -> do
          schedule
            Scheduler{waiting = sleep action1 (sleep action2 rest), ..}

runThreads :: Threads m a -> (a -> Action m) -> Action m
runThreads = coerce

instance Monad m => Functor (Threads m) where
  fmap = liftM

instance Monad m => Applicative (Threads m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (Threads m) where
  return x = Threads (\k -> k x)
  m >>= f  = Threads $ \k -> runThreads m (\a -> runThreads (f a) k)

instance MonadTrans Threads where
  lift m = Threads $ \k -> Lift (fmap k m)
