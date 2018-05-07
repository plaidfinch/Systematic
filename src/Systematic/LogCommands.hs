{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Systematic.LogCommands
  ( LogCommands, logCommandsWith
  ) where

import Systematic.Language

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch

import Data.Coerce
import Data.Typeable

import Prelude hiding (log)


newtype LogCommands m a
  = LogCommands (ReaderT (String -> m ()) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO)

withLogger :: ((String -> m ()) -> m a) -> LogCommands m a
withLogger = coerce

logCommandsWith :: (String -> m ()) -> LogCommands m a -> m a
logCommandsWith logger action =
  coerce action logger

instance MonadTrans LogCommands where
  lift = LogCommands . lift

logCommand
  :: MonadCatch m => (a -> Maybe String) -> m a -> String -> LogCommands m a
logCommand showResult action commandString =
  withLogger $ \logger ->
    catch
      (do result <- action
          logSuccess logger result
          return result)
      (\(exception :: SomeException) -> do
         logFailure logger exception
         throwM exception)
  where
    logSuccess logger result =
      logger $ maybe "" (++ " <- ") (showResult result) ++ commandString

    logFailure logger e =
      logger (commandString ++ "\n-- *** Exception: " ++ show e)

name :: Show b => String -> (a -> b) -> a -> String
name prefix convert = (prefix ++) . show . convert

nameThread  = name "t" threadId
nameRef     = name "r" refId
nameVar     = name "v" varId
nameChannel = name "c" channelId
nameSocket  = name "s" socketId

instance (HasThreads m, MonadCatch m) => HasThreads (LogCommands m) where
  type ThreadId (LogCommands m) = ThreadId m
  fork process =
    withLogger $ \logger -> do
      tid <- fork (logCommandsWith logger process)
      logger ("-- Forked thread " ++ nameThread tid)
      return tid
  kill tid =
    logCommand don't_show (kill tid) $
      concat [ "kill "
             , nameThread tid
             ]

showWithParens :: Show a => a -> String
showWithParens a = showsPrec 11 a ""

don't_show :: a -> Maybe String
don't_show = const Nothing

just_show :: Show a => a -> Maybe String
just_show = Just . show

instance (HasLog m, MonadCatch m) => HasLog (LogCommands m) where
  log message =
    logCommand don't_show
      (log message) $
      concat [ "log @"
             , showWithParens (typeOf message)
             , " "
             , showWithParens message
             ]

instance (HasMemory m, MonadCatch m) => HasMemory (LogCommands m) where
  type Ref (LogCommands m) = Ref m
  newRef = logCommand (Just . nameRef) newRef "newRef"
  readRef ref =
    logCommand just_show
      (readRef ref) $
      concat [ "readRef "
             , nameRef ref
             ]
  writeRef ref val =
    logCommand don't_show
      (writeRef ref val) $
      concat [ "writeRef "
             , nameRef ref
             , showWithParens val
             ]

  type Var (LogCommands m) = Var m
  newVar = logCommand (Just . nameVar) newVar "newVar"
  takeVar var =
    logCommand just_show
      (takeVar var) $
      concat [ "takeVar "
             , nameVar var
             ]
  putVar var val =
    logCommand don't_show
      (putVar var val) $
      concat [ "putVar "
             , nameVar var
             , showWithParens val
             ]

  type Channel (LogCommands m) = Channel m
  newChan = logCommand (Just . nameChannel) newChan "newChan"
  readChan chan =
    logCommand just_show
      (readChan chan) $
      concat [ "readChan "
             , nameChannel chan
             ]
  writeChan chan val =
    logCommand don't_show
      (writeChan chan val) $
      concat [ "writeChan "
             , nameChannel chan
             , showWithParens val
             ]

instance (HasSockets m, MonadCatch m) => HasSockets (LogCommands m) where
  type Socket (LogCommands m) = Socket m
  connect transport addressType address port =
    logCommand (Just . nameSocket)
      (connect transport addressType address port) $
      concat [ "connect "
             , showWithParens transport
             , " "
             , showWithParens addressType
             , " "
             , showAddress addressType address
             , " "
             , showWithParens port
             ]
  listen addressType port =
    logCommand (Just . nameSocket)
      (listen addressType port) $
      concat [ "listen "
             , show addressType
             , " "
             , showWithParens port
             ]
  accept socket =
    logCommand (Just . nameSocket)
      (accept socket) $
      concat [ "accept "
             , nameSocket socket
             ]
  send socket string =
    logCommand don't_show
      (send socket string) $
      concat [ "send "
             , nameSocket socket
             , " "
             , showWithParens string
             ]
  receive socket =
    logCommand just_show
      (receive socket) $
      concat [ "receive "
             , nameSocket socket
             ]
  receiveUntil char socket =
    logCommand just_show
      (receiveUntil char socket) $
      concat [ "receiveUntil "
             , showWithParens char
             , " "
             , nameSocket socket
             ]
  close socket =
    logCommand don't_show
      (close socket) $
      concat [ "close "
             , nameSocket socket
             ]
