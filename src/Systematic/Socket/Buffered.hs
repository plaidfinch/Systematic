module Systematic.Socket.Buffered
  ( Socket
  , connect
  , listen
  , accept
  , send
  , sendLine
  , receive
  , receiveUntil
  , receiveLine
  , close
  ) where

import Systematic.Language hiding (HasSockets(..), sendLine)
import Systematic.Language (HasSockets)
import qualified Systematic.Socket.Raw as Raw

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.Function


data Socket m f t (mode :: Mode)
  = Socket
      { socketBuffer      :: Var m ByteString
      , wrappedSocket     :: Raw.Socket m f t mode
      }

wrapSocket :: HasMemory m => Raw.Socket m f t mode -> m (Socket m f t mode)
wrapSocket socket = do
  buffer <- newVar BS.empty
  return
    Socket
      { wrappedSocket = socket
      , socketBuffer = buffer }


-- Receiving is the only thing different in implementation for a buffered socket

receive
  :: (HasBlockingMemory m, HasSockets m)
  => Socket m f t Connected -> m ByteString
receive Socket{wrappedSocket, socketBuffer} = do
  buffered <- takeVar socketBuffer
  if buffered /= ""
    then do
      putVar socketBuffer ""
      return buffered
    else do
      received <- Raw.receive wrappedSocket
      putVar socketBuffer ""
      return received

receiveUntil
  :: (HasBlockingMemory m, HasSockets m)
  => Char -> Socket m f t Connected -> m (Maybe ByteString)
receiveUntil char Socket{wrappedSocket, socketBuffer} = do
  buffered <- takeVar socketBuffer
  case maybeSplit char buffered of
    Just (before, after) -> do
      putVar socketBuffer after
      return (Just before)
    Nothing ->
      (fmap . fmap) (BS.concat . (buffered :)) $
        fix $ \again -> do
          received <- Raw.receive wrappedSocket
          if received == "" then return Nothing else
            case maybeSplit char received of
              Just (before, after) -> do
                putVar socketBuffer after
                return (Just [before])
              Nothing -> do
                fmap (received :) <$> again

receiveLine
  :: (HasBlockingMemory m, HasSockets m)
  => Socket m f t Connected
  -> m (Maybe ByteString)
receiveLine =
  receiveUntil '\n'


-- Lifting raw socket commands onto buffered sockets

connect
  :: (HasMemory m, HasSockets m)
  => Transport t -> AddressType f -> Address f -> Port
  -> m (Socket m f t Connected)
connect transport addressType address port =
  wrapSocket =<< Raw.connect transport addressType address port

listen
  :: (HasMemory m, HasSockets m)
  => AddressType f -> Port -> m (Socket m f TCP Listening)
listen addressType port =
  wrapSocket =<< Raw.listen addressType port

accept
  :: (HasMemory m, HasSockets m)
  => Socket m f TCP Listening -> m (Socket m f TCP Connected)
accept Socket{wrappedSocket} =
  wrapSocket =<< Raw.accept wrappedSocket

send :: HasSockets m => Socket m f t Connected -> ByteString -> m ()
send Socket{wrappedSocket} string =
  Raw.send wrappedSocket string

sendLine
  :: HasSockets m
  => Socket m f t Connected -> ByteString
  -> m ()
sendLine Socket{wrappedSocket} string =
  Raw.sendLine wrappedSocket string

close :: HasSockets m => Socket m f t mode -> m ()
close Socket{wrappedSocket} =
  Raw.close wrappedSocket


-- Utility functions

maybeSplit :: Char -> ByteString -> Maybe (ByteString, ByteString)
maybeSplit char string =
  if BS.length suffix > 0
  then Just (prefix, BS.drop 1 suffix)
  else Nothing
  where
    (prefix, suffix) = BS.break (== char) string
