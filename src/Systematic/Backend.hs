module Systematic.Backend
  ( Backend(..)
  ) where

import Systematic
import Systematic.Logging

import qualified Systematic.Backend.IO as IO

class Backend m where
  runProcess
    :: LocalLogger m
    -> (forall socket. Process socket a)
    -> m (Maybe a)

instance Backend IO where runProcess = IO.runProcess
