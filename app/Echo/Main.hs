module Main where

import Systematic
import qualified Systematic.Real as Real

import Control.Monad
import Control.Monad.IO.Class

main :: IO ()
main = Real.run
     . Real.sockets
     . logCommandsWith (liftIO . putStrLn)
     $ echoServer (Port 10000)

echoServer :: (HasThreads m, HasSockets m) => Port -> m ()
echoServer port = do
  s <- listen IPv4 port
  forever $ do
    s' <- accept s
    fork (echo s')
  where
    echo s =
      receiveLine s >>= \case
        Nothing   -> close s
        Just line -> sendLine s line >> echo s
