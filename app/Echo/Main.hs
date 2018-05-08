module Main where

import Systematic
import Systematic.Socket.Buffered
import qualified Systematic.Backend.Real as Real

import Control.Monad
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case read <$> args of
    [port] ->
      realRun (echoServer (Port port))
    _ -> do
      putStrLn "usage: echo [port number]"
      exitFailure

realRun :: (forall m. Backend m => m ()) -> IO ()
realRun =
  Real.run . Real.sockets . logCommands . Real.memory

echoServer :: Backend m => Port -> m ()
echoServer port = do
  listening <- listen IPv4 port
  forever $ do
    connection <- accept listening
    fork (echoWith connection)
  where
    echoWith connection = do
      maybeLine <- receiveLine connection
      case maybeLine of
        Nothing   -> close connection
        Just line -> do
          sendLine connection line
          echoWith connection
