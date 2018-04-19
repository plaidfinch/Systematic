module Systematic.Backend.IO
  ( runProcess
  ) where

import Systematic
import Systematic.Logging
import Systematic.Enumerator

import qualified System.Socket as S
import qualified System.Socket.Family.Inet as S
import qualified System.Socket.Family.Inet6 as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream as S

import Control.Monad.Operational
import Data.Functor
import Data.Function
import Control.Exception
import Data.Monoid

import Data.IORef
import Control.Concurrent
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Implementation against actual network sockets

actualAddress :: AddressType f -> Address f -> Port -> S.SocketAddress f
actualAddress IPv4 tuple (Port p) =
  S.SocketAddressInet
    (S.inetAddressFromTuple tuple)
    (fromIntegral p)
actualAddress IPv6 tuple (Port p) =
  S.SocketAddressInet6
    (S.inet6AddressFromTuple tuple)
    (fromIntegral p) 0 0

withAddressFamily :: AddressType f -> (S.Family f => r) -> r
withAddressFamily IPv4 k = k
withAddressFamily IPv6 k = k

withTransportType :: Transport t -> (S.Type t => r) -> r
withTransportType TCP k = k
withTransportType UDP k = k


-- An actual socket, attached to a mutable buffer
data Socket f t (mode :: Mode) where
  Socket
    :: S.Family f
    => { socketIdentity    :: SocketId
       , socketTransport   :: Transport t
       , socketRequestSize :: IORef Int
       , socketBuffer      :: MVar ByteString
       , socketHandle      :: S.Socket f t S.Default
       } -> Socket f t mode

instance SocketInfo Socket where
  socketId (Socket{socketIdentity}) = socketIdentity

setupSocket
  :: forall f t mode. S.Family f
  => Enumerator SocketId
  -> Transport t
  -> AddressType f
  -> IO (Socket f t mode)
setupSocket ids transport addressType =
  withTransportType transport $ do
    socket <- S.socket @f @t @S.Default
    S.setSocketOption socket (S.ReuseAddress True)
    case addressType of
      IPv6 -> S.setSocketOption socket (S.V6Only False) :: IO ()
      _    -> mempty :: IO ()
    wrapSocket ids transport socket

defaultSocketRequestSize :: Int
defaultSocketRequestSize = 128

wrapSocket
  :: S.Family f
  => Enumerator SocketId
  -> Transport t
  -> S.Socket f t S.Default
  -> IO (Socket f t mode)
wrapSocket ids transport socket = do
  uniqueID <- next ids
  buffer   <- newMVar BS.empty
  requestSize <- newIORef defaultSocketRequestSize
  return
    Socket{ socketIdentity    = uniqueID
          , socketTransport   = transport
          , socketRequestSize = requestSize
          , socketBuffer      = buffer
          , socketHandle      = socket }

runProcess
  :: LocalLogger IO
  -> (forall socket. Process socket a)
  -> IO (Maybe a)
runProcess LocalLogger{logEvent} (Process program) = do
  socketIds <- enumerateFrom (SocketId 0)
  runMaybeT $ interpretWithMonad (evalLogging socketIds) program
  where
    evalLogging :: Enumerator SocketId -> SysCall Socket a -> MaybeT IO a
    evalLogging socketIds syscall =
      MaybeT $ catch
        (do result <- eval socketIds syscall
            logEvent (LocalEvent syscall (Right result))
            return (Just result))
        (\(e :: S.SocketException) ->
           do logEvent (LocalEvent syscall (Left e))
              return Nothing)

    eval :: Enumerator SocketId -> SysCall Socket a -> IO a
    eval socketIds = \case
      Connect transport addressType address port ->
        withAddressFamily addressType $ do
          socket@Socket{socketHandle = s} <-
            setupSocket socketIds transport addressType
          S.connect s (actualAddress addressType address port)
          return socket

      Listen (addressType :: AddressType f) (Port port) ->
        withAddressFamily addressType $ do
          socket@Socket{socketHandle = s} <-
            setupSocket socketIds TCP addressType
          let socketAddress :: S.SocketAddress f
              socketAddress =
                case addressType of
                  IPv4 -> S.SocketAddressInet
                    S.inetAny (fromIntegral port)
                  IPv6 -> S.SocketAddressInet6
                    S.inet6Any (fromIntegral port) 0 0
          S.bind s socketAddress
          S.listen s 0
          return socket

      Accept Socket{socketTransport, socketHandle = s} -> do
        (s', _) <- S.accept s
        wrapSocket socketIds socketTransport s'

      Send Socket{socketTransport, socketHandle = s} string ->
        case socketTransport of
          TCP -> void $ S.sendAll s string S.msgNoSignal
          UDP -> void $ S.send    s string S.msgNoSignal

      Receive Socket{socketBuffer, socketRequestSize, socketHandle = s} -> do
        buffered <- takeMVar socketBuffer
        let bufferSize = BS.length buffered
        requestSize <- readIORef socketRequestSize
        if bufferSize >= requestSize
          then do
            let (requested, remainder) =
                  BS.splitAt bufferSize buffered
            putMVar socketBuffer remainder
            return requested
          else do
            received <- S.receive s requestSize S.msgNoSignal
            putMVar socketBuffer ""
            return (buffered <> received)

      ReceiveUntil char
        Socket{socketBuffer, socketRequestSize, socketHandle = s} -> do
          buffered <- takeMVar socketBuffer
          requestSize <- readIORef socketRequestSize
          case maybeSplit char buffered of
            Just (before, after) -> do
              putMVar socketBuffer after
              return before
            Nothing ->
              fmap (BS.concat . (buffered :)) $
                fix $ \again -> do
                  received <- S.receive s requestSize S.msgNoSignal
                  case maybeSplit char received of
                    Just (before, after) -> do
                      putMVar socketBuffer after
                      return [before]
                    Nothing -> do
                      (received :) <$> again

      Close Socket{socketHandle = s} ->
        S.close s

      LogMessage _ ->
        mempty

maybeSplit :: Char -> ByteString -> Maybe (ByteString, ByteString)
maybeSplit char string =
  if BS.length suffix > 0
  then Just (prefix, BS.drop 1 suffix)
  else Nothing
  where
    (prefix, suffix) = BS.break (== char) string
