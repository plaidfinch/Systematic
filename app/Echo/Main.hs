module Main where

import Systematic                                 -- the main library
import Systematic.Socket.Buffered                 -- use line-buffered sockets
import qualified Systematic.Backend.Real as Real  -- use "real world" backend

import Control.Monad      (forever)               -- run an action forever
import System.Environment (getArgs)               -- get command-line args
import System.Exit        (exitFailure)           -- exit program with an error
import System.IO          (hPutStrLn, stderr)     -- print to stderr
import Text.Read          (readMaybe)             -- read, but fail with Nothing


-- The main function: read command-line args, then start the server
main :: IO ()
main = do
  args <- getArgs                          -- get a list of command-line args
  case map readMaybe args of               -- try to read them as port #s
    [Just port] ->                         -- if just one well-formatted port,
      realRun (echoServer (Port port))     -- run the echo server on that port;
    _ -> usageMessage                      -- otherwise, print the usage message
  where
    usageMessage = do
      hPutStrLn stderr "usage: echo [port number]"  -- print to stderr
      exitFailure                                   -- exit with an error

-- An abstract description of an echo server (works for any choice of backend)
echoServer :: Port -> Program ()
echoServer port = do
  listening <- listen IPv4 port            -- listen on the port via IPv4,
  forever $ do                             -- then in a loop...
    connection <- accept listening         -- accept a new connection, and
    fork (echoWith connection)             -- fork a thread to interact with it
  where
    echoWith connection = do
      maybeLine <- receiveLine connection  -- receive a line over the connection
      case maybeLine of
        Nothing ->                         -- if received nothing, user is done:
          close connection                 -- close the connection and finish;
        Just line -> do                    -- else if user sent a line of input,
          sendLine connection line         -- send it back, and
          echoWith connection              -- loop again, waiting for more

-- One choice of backend: this is how we'll run our program this time
realRun :: Program a -> IO a
realRun =
  Real.base       -- run using the "real" backend (which includes threading),
  . Real.sockets  -- using real network sockets,
  . Real.memory   -- using real mutable memory to implement buffers & such,
  . logCommands   -- logging all executed commands to stdout
