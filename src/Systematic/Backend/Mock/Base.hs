module Systematic.Backend.Mock.Base where

import Systematic.Language

import Control.Monad.ST
import Control.Monad.Fix
import Control.Monad.Catch.Pure

newtype Base s a
  = Base (CatchT (ST s) a)
  deriving newtype
    ( Functor, Applicative, Monad,
      MonadFix, MonadCatch, MonadThrow )

base :: (forall s. Base s a) -> Either SomeException a
base action =
  runST (runCatchT (getBase action))
  where
    getBase :: (forall s. Base s a) -> CatchT (ST s') a
    getBase (Base a) = a
