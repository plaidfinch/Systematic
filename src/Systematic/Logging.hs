module Systematic.Logging
  ( SocketId(..)
  , ThreadId(..)
  , SocketInfo(..)
  , LocalLogger(..)
  , LocalEvent(..)
  , logAsHaskell
  ) where

import Systematic.Language
import Systematic.CError

import System.Socket (SocketException(..))
import Data.Typeable

-- For printing and logging purposes, every socket should have a unique ID
newtype SocketId
  = SocketId Integer
  deriving (Eq, Ord, Show, Enum)

-- We require that any runnable thing have such a unique ID
class SocketInfo socket where
  socketId :: socket f t mode -> SocketId

-- How to log local events?
newtype LocalLogger m
  = LocalLogger
      { logEvent
          :: forall socket. SocketInfo socket
          => ThreadId -> LocalEvent socket -> m () }

-- A local event is a syscall paired with either its result or an exception
data LocalEvent socket where
  LocalEvent
    :: SysCall socket a
    -> Either SocketException a
    -> LocalEvent socket

logAsHaskell
  :: Monad m
  => (String -> m ())
  -> (forall a. (Typeable a, Show a) => a -> m ())
  -> LocalLogger m
logAsHaskell localLog messageLog =
  LocalLogger $ \threadId -> \case
    LocalEvent syscall result -> do
      localLog (prettySysCall (("s" ++) . show) threadId syscall result)
      case syscall of
        LogMessage message -> messageLog message
        _ -> return ()

prettySysCall
  :: SocketInfo socket
  => (Integer -> String)
  -> ThreadId
  -> SysCall socket a
  -> Either SocketException a
  -> String
prettySysCall socketName (ThreadId threadId) syscall result =
  let outputLines = lines $ prettyCall ++ maybeExceptionComment
  in unlines $ map (++ ("    -- " ++ show threadId)) outputLines
  where
    prettyCall = case syscall of
      Connect transport addressType address port ->
        concat [ maybeBind nameSocket result
               , "connect "
               , show transport
               , " "
               , show addressType
               , " "
               , showAddress addressType address
               , " ("
               , show port
               , ")"
               ]
      Listen addressType port ->
        concat [ maybeBind nameSocket result
               , "listen "
               , show addressType
               , " ("
               , show port
               , ")"
               ]
      Accept socket ->
        concat [ maybeBind nameSocket result
               , "accept "
               , nameSocket socket
               ]
      Send socket string ->
        concat [ "send "
               , nameSocket socket
               , " "
               , show string
               ]
      Receive socket ->
        concat [ maybeBind show result
               , "receive "
               , nameSocket socket
               ]
      ReceiveUntil char socket ->
        concat [ maybeBind show result
               , "receiveUntil "
               , show char
               , " "
               , nameSocket socket
               ]
      Close socket ->
        concat [ "close "
               , nameSocket socket
               ]
      Fork _ ->
        "-- Forked thread " ++ show result
      LogMessage message ->
        concat [ "logMessage @"
               , showsPrec 11 (typeOf message) ""
               , " "
               , showsPrec 11 message ""
               ]

    maybeExceptionComment = case result of
      Right{} -> ""
      Left (SocketException errorCode) ->
        "\n-- *** Error " ++ show errorCode ++ ": " ++ describeCError errorCode

    maybeBind :: (r -> String) -> Either e r -> String
    maybeBind display =
      either (const "") ((++ " <- ") . display)

    nameSocket :: SocketInfo socket => socket f t mode -> String
    nameSocket (socketId -> SocketId i) = socketName i
