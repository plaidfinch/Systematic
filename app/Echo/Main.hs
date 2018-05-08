module Main where

import Systematic
import Systematic.Socket.Buffered
import qualified Systematic.Backend.Real as Real

import Control.Monad
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case read <$> args of
    [port] -> realRun (echoServer (Port port))
    _      -> usageMessage
  where
    usageMessage = do
      hPutStrLn stderr "usage: echo [port number]"
      exitFailure

echoServer :: Port -> Program ()
echoServer port = do
  listening <- listen IPv4 port
  forever $ do
    connection <- accept listening
    fork (echoWith connection)
  where
    echoWith connection = do
      maybeLine <- receiveLine connection
      case maybeLine of
        Nothing -> close connection
        Just line -> do
          sendLine connection line
          echoWith connection

realRun :: Program a -> IO a
realRun =
  Real.run . Real.sockets . logCommands . Real.memory
