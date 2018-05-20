{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Systematic.LogCommands
  ( LogCommands, logCommandsWith, logCommands
  ) where

import Systematic.Language

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Catch
import Control.Monad.Fix

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Coerce
import Data.Typeable

import Data.Maybe

import Prelude hiding (log)

data LogState m
  = LogState
      { processLine :: String -> String
      , nameState   :: Ref m (Map String Int)
      }

newtype LogCommands m a
  = LogCommands (ReaderT (LogState m) m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadFix
    , HasMemory, HasSync )

logCommands :: (HasMemory m, HasTextLog m) => LogCommands m a -> m a
logCommands action = do
  appendLogString "trace = do"
  logCommandsWith ("  " ++) action

logCommandsWith :: HasMemory m => (String -> String) -> LogCommands m a -> m a
logCommandsWith processLine action = do
  nameState <- newRef Map.empty
  runLogCommands action LogState{nameState, processLine}

runLogCommands :: LogCommands m a -> LogState m -> m a
runLogCommands action logState =
  coerce action logState

instance MonadTrans LogCommands where
  lift = LogCommands . lift

-- To log actions, we need to be able to keep track of things' names

data Named t
  = Named
      { prefix   :: String
      , count    :: Int
      , nameless :: t }

nameOf :: Named t -> String
nameOf Named{prefix, count} =
  prefix ++ show count

sendLog :: HasTextLog m => String -> LogCommands m ()
sendLog string =
  LogCommands $ do
    logger <- asks processLine
    appendLogString (logger string)

getLogState :: Monad m => LogCommands m (LogState m)
getLogState = LogCommands ask

name :: HasMemory m => String -> a -> LogCommands m (Named a)
name prefix nameless =
  LogCommands $ do
    state <- asks nameState
    modifyRef state $ \old ->
      let count = fromMaybe 0 $ Map.lookup prefix old
      in ( Map.insert prefix (succ count) old
         , Named{prefix, count, nameless} )

-- Naming parameterized types

-- newtype Named1 t a
--   = Named1 (Named (t a))

-- nameOf1 :: Named1 t a -> String
-- nameOf1 (Named1 x) = nameOf x

-- name1 :: HasMemory m => String -> t a -> LogCommands m (Named1 t a)
-- name1 prefix nameless =
--   coerce <$> name prefix nameless

-- nameless1 :: Named1 t a -> t a
-- nameless1 = nameless . coerce

-- newtype Named2 t a b
--   = Named2 (Named (t a b))

-- nameOf2 :: Named2 t a b -> String
-- nameOf2 (Named2 x) = nameOf x

-- name2 :: HasMemory m => String -> t a b -> LogCommands m (Named2 t a b)
-- name2 prefix nameless =
--   coerce <$> name prefix nameless

-- nameless2 :: Named2 t a b -> t a b
-- nameless2 = nameless . coerce

newtype Named3 t a b c
  = Named3 (Named (t a b c))

nameOf3 :: Named3 t a b c -> String
nameOf3 (Named3 x) = nameOf x

name3 :: HasMemory m => String -> t a b c -> LogCommands m (Named3 t a b c)
name3 prefix nameless =
  coerce <$> name prefix nameless

nameless3 :: Named3 t a b c -> t a b c
nameless3 = nameless . coerce

-- Logging commands

logCommand
  :: (HasTextLog m, MonadCatch m)
  => (a -> Maybe String) -> LogCommands m a -> String -> LogCommands m a
logCommand showResult action commandString = do
  LogState{processLine} <- getLogState
  catch
    (do result <- action
        lift (logSuccess processLine result))
    (\(exception :: SomeException) ->
        lift (logFailure processLine exception))
  where
    logSuccess logger result = do
      appendLogString . logger $
        maybe "" (++ " <- ") (showResult result) ++ commandString
      return result

    logFailure logger exception = do
      appendLogString $
        logger commandString ++ "\n" ++
        logger ("-- *** Exception: " ++ show exception)
      throwM exception

padRight :: Int -> String -> String
padRight n string =
  string ++ replicate (n - length string) ' '

-- Specialized functions to name different things

nameThread :: HasMemory m => ThreadId m -> LogCommands m (ThreadId (LogCommands m))
nameThread = name "t"

-- nameRef :: HasMemory m => Ref m a -> LogCommands m (Ref (LogCommands m) a)
-- nameRef = name1 "r"

-- nameVar :: HasMemory m => Var m a -> LogCommands m (Var (LogCommands m) a)
-- nameVar = name1 "v"

-- nameChan :: HasMemory m => Channel m a -> LogCommands m (Channel (LogCommands m) a)
-- nameChan = name1 "c"

nameSocket :: HasMemory m => Socket m f t mode -> LogCommands m (Socket (LogCommands m) f t mode)
nameSocket = name3 "s"

instance (HasThreads m, HasMemory m, HasTextLog m, MonadCatch m, MonadFix m)
  => HasThreads (LogCommands m) where

  type ThreadId (LogCommands m) = Named (ThreadId m)
  fork process = mdo
      LogState{processLine, nameState} <- getLogState
      let suffixTid =
            processLine . (++ (" -- " ++ nameOf tid)) . padRight 60
      tid <- nameThread =<<
               lift (fork (runLogCommands process
                             LogState{nameState, processLine = suffixTid}))
      sendLog ("-- Forked thread " ++ nameOf tid)
      return tid
  kill tid =
    logCommand don't_show
      (lift $ kill (nameless tid)) $
      concat [ "kill "
             , nameOf tid
             ]
  yield =
    logCommand don't_show yield $ "yield"

showWithParens :: Show a => a -> String
showWithParens a = showsPrec 11 a ""

don't_show :: a -> Maybe String
don't_show = const Nothing

just_show :: Show a => a -> Maybe String
just_show = Just . show

instance (HasLog m, HasTextLog m, MonadCatch m) => HasLog (LogCommands m) where
  log message =
    logCommand don't_show
      (log message) $
      concat [ "log @"
             , showWithParens (typeOf message)
             , " "
             , showWithParens message
             ]

-- TODO: How to log memory? Add manual comment ability to logs?

-- instance (HasMemory m, HasTextLog m, MonadCatch m)
--   => HasMemory (LogCommands m) where

--   type Ref (LogCommands m) = Named1 (Ref m)
--   newRef val =
--     logCommand
--       (Just . nameOf1)
--       (nameRef =<< lift (newRef val)) $
--       concat [ "newRef "
--              , show val
--              ]
--   readRef ref =
--     logCommand just_show
--       (lift $ readRef (nameless1 ref)) $
--       concat [ "readRef "
--              , nameOf1 ref
--              ]
--   writeRef ref val =
--     logCommand don't_show
--       (lift $ writeRef (nameless1 ref) val) $
--       concat [ "writeRef "
--              , nameOf1 ref
--              , " "
--              , showWithParens val
--              ]
--   modifyRef ref function =
--     logCommand just_show
--       (lift $ modifyRef (nameless1 ref) function) $
--       concat [ "modifyRef "
--              , nameOf1 ref
--              , " _____  -- [cannot show functions]"
--              ]

--   type Var (LogCommands m) = Named1 (Var m)
--   newVar val =
--     logCommand
--       (Just . nameOf1)
--       (nameVar =<< lift (newVar val)) $
--       concat [ "newVar "
--              , show val
--              ]
--   newEmptyVar =
--     logCommand
--       (Just . nameOf1)
--       (nameVar =<< lift newEmptyVar)
--       "newEmptyVar"
--   takeVar var =
--     logCommand just_show
--       (lift $ takeVar (nameless1 var)) $
--       concat [ "takeVar "
--              , nameOf1 var
--              ]
--   putVar var val =
--     logCommand don't_show
--       (lift $ putVar (nameless1 var) val) $
--       concat [ "putVar "
--              , nameOf1 var
--              , " "
--              , showWithParens val
--              ]

--   type Channel (LogCommands m) = Named1 (Channel m)
--   newChan =
--     logCommand
--       (Just . nameOf1)
--       (nameChan =<< lift newChan)
--       "newChan"
--   readChan chan =
--     logCommand just_show
--       (lift $ readChan (nameless1 chan)) $
--       concat [ "readChan "
--              , nameOf1 chan
--              ]
--   writeChan chan val =
--     logCommand don't_show
--       (lift $ writeChan (nameless1 chan) val) $
--       concat [ "writeChan "
--              , nameOf1 chan
--              , " "
--              , showWithParens val
--              ]

instance (HasSockets m, HasMemory m, HasTextLog m, MonadCatch m)
  => HasSockets (LogCommands m) where

  type Socket (LogCommands m) = Named3 (Socket m)
  connect transport addressType address port =
    logCommand (Just . nameOf3)
      (nameSocket =<< lift (connect transport addressType address port)) $
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
    logCommand (Just . nameOf3)
      (nameSocket =<< lift (listen addressType port)) $
      concat [ "listen "
             , show addressType
             , " "
             , showWithParens port
             ]
  accept socket =
    logCommand (Just . nameOf3)
      (nameSocket =<< lift (accept (nameless3 socket))) $
      concat [ "accept "
             , nameOf3 socket
             ]
  send socket string =
    logCommand don't_show
      (lift $ send (nameless3 socket) string) $
      concat [ "send "
             , nameOf3 socket
             , " "
             , showWithParens string
             ]
  receive socket =
    logCommand just_show
      (lift $ receive (nameless3 socket)) $
      concat [ "receive "
             , nameOf3 socket
             ]
  close socket =
    logCommand don't_show
      (lift $ close (nameless3 socket)) $
      concat [ "close "
             , nameOf3 socket
             ]
