module Main where

import Systematic
import qualified Systematic.Backend.Real as Real

import Control.Monad

main :: IO ()
main = Real.run
     . Real.sockets
     -- . logCommands
     . Real.memory
     $ echoServer (Port 10000)

echoServer :: Backend m => Port -> m ()
echoServer port = do
  listening <- listen IPv4 port
  forever $ do
    connection <- accept listening
    fork (echo connection)
  where
    echo connection = do
      maybeLine <- receiveLine connection
      case maybeLine of
        Nothing   -> close connection
        Just line -> do
          sendLine connection line
          echo connection
