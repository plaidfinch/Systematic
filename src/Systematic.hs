{-# language StrictData #-}

module Systematic
  ( Mode(..)
  , Transport(..)
  , Network
  , connect
  , listen
  , accept
  , send
  , receive
  , close
  , runSystem
  ) where

import Data.ByteString
import Data.Binary
import Control.Monad.Operational as O
import GHC.Generics
import Data.Functor
import Data.IORef

import qualified System.Socket                  as Socket
import qualified System.Socket.Family.Inet      as Socket
import qualified System.Socket.Family.Inet6     as Socket
import qualified System.Socket.Type.Stream      as Socket
import qualified System.Socket.Type.Datagram    as Socket
import qualified System.Socket.Protocol.Default as Socket

data Mode
  = Listening
  | Connected

data Transport t where
  TCP :: Transport Socket.Stream
  UDP :: Transport Socket.Datagram

data Network socket a where
  Connect
    :: Transport t
    -> Address f
    -> Port
    -> Network socket (socket f t Connected)
  Listen
    :: Transport t
    -> Address f
    -> Port
    -> Network socket (socket f t Listening)
  Accept
    :: socket f t Listening
    -> Network socket (socket f t Connected)
  Send
    :: socket f t Connected
    -> ByteString
    -> Network socket ()
  Receive
    :: socket f t Connected
    -> Int
    -> Network socket ByteString
  Close
    :: socket f t mode
    -> Network socket ()

type Process a
  = forall socket. Program (Network socket) a

connect
  :: Transport t
  -> Address f
  -> Port
  -> Program (Network socket) (socket f t Connected)
connect transport address port =
  O.singleton (Connect transport address port)

listen
  :: Transport t
  -> Address f
  -> Port
  -> Program (Network socket) (socket f t Listening)
listen transport address port =
  O.singleton (Listen transport address port)

accept
  :: socket f t Listening
  -> Program (Network socket) (socket f t Connected)
accept socket =
  O.singleton (Accept socket)

send
  :: socket f t Connected
  -> ByteString
  -> Program (Network socket) ()
send socket bytestring =
  O.singleton (Send socket bytestring)

receive
  :: socket f t Connected
  -> Int
  -> Program (Network socket) ByteString
receive socket bufferSize =
  O.singleton (Receive socket bufferSize)

close
  :: socket f t mode
  -> Program (Network socket) ()
close socket =
  O.singleton (Close socket)

-- Implementation against actual network sockets

data Address f where
  IPv4 :: Word8 -> Word8 -> Word8 -> Word8
       -> Address Socket.Inet
  IPv6 :: Word16 -> Word16 -> Word16 -> Word16
       -> Word16 -> Word16 -> Word16 -> Word16
       -> Address Socket.Inet6

deriving instance Eq (Address f)
deriving instance Ord (Address f)
deriving instance Show (Address f)
-- need a Binary instance for Address

actualAddress :: Address f -> Port -> Socket.SocketAddress f
actualAddress (IPv4 a b c d) (Port p) =
  Socket.SocketAddressInet
    (Socket.inetAddressFromTuple (a, b, c, d))
    (fromIntegral p)
actualAddress (IPv6 a b c d e f g h) (Port p) =
  Socket.SocketAddressInet6
    (Socket.inet6AddressFromTuple (a, b, c, d, e, f, g, h))
    (fromIntegral p) 0 0

withAddressFamily :: Address f -> (Socket.Family f => r) -> r
withAddressFamily IPv4{} k = k
withAddressFamily IPv6{} k = k

withTransportType :: Transport t -> (Socket.Type t => r) -> r
withTransportType TCP k = k
withTransportType UDP k = k

newtype Port
  = Port Word16
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (Binary)
  deriving newtype (Show, Num)

newtype SocketID
  = SocketID Integer
  deriving (Eq, Ord, Show, Enum)

data ActualSocket f t (mode :: Mode) where
  ASocket
    :: Socket.Family f
    => SocketID
    -> Transport t
    -> Socket.Socket f t Socket.Default
    -> ActualSocket f t mode

newtype IDStream a
  = IDStream (IORef a)

newIDStream :: a -> IO (IDStream a)
newIDStream a = do
  ref <- newIORef a
  return (IDStream ref)

newID :: Enum a => IDStream a -> IO a
newID (IDStream ref) = do
  atomicModifyIORef ref (\(succ -> a) -> (a, a))

setupSocket
  :: forall f t mode. Socket.Family f
  => IDStream SocketID
  -> Transport t
  -> IO (ActualSocket f t mode)
setupSocket ids (transport :: Transport t) =
  withTransportType transport $ do
    i <- newID ids
    s <- Socket.socket @f @t @Socket.Default
    Socket.setSocketOption s (Socket.ReuseAddress True)
    Socket.setSocketOption s (Socket.V6Only False)
    return (ASocket i transport s)

data NetworkEvent socket where
  NetworkEvent :: Network socket a -> a -> NetworkEvent socket

runSystem :: (NetworkEvent ActualSocket -> IO ()) -> Process a -> IO a
runSystem logEvent program = do
  socketIDs <- newIDStream (SocketID 0)
  interpretWithMonad (evalLogging socketIDs) (program @ActualSocket)
  where
    evalLogging :: IDStream SocketID -> Network ActualSocket a -> IO a
    evalLogging socketIDs syscall = do
      result <- eval socketIDs syscall
      logEvent (NetworkEvent syscall result)
      return result

    eval :: IDStream SocketID -> Network ActualSocket a -> IO a
    eval socketIDs = \case
      Connect transport address port ->
        withAddressFamily address $ do
          socket@(ASocket _ _ s) <- setupSocket socketIDs transport
          Socket.connect s (actualAddress address port)
          return socket

      Listen transport address port ->
        withAddressFamily address $ do
          socket@(ASocket _ _ s) <- setupSocket socketIDs transport
          Socket.bind s (actualAddress address port)
          Socket.listen s 0
          return socket

      Accept (ASocket _ transport s) -> do
        (s', _) <- Socket.accept s
        i <- newID socketIDs
        return (ASocket i transport s')

      Send (ASocket _ transport s) string ->
        case transport of
          TCP -> void $ Socket.sendAll s string Socket.msgNoSignal
          UDP -> void $ Socket.send    s string Socket.msgNoSignal

      Receive (ASocket _ _ s) bufferSize ->
        Socket.receive s bufferSize Socket.msgNoSignal

      Close (ASocket _ _ s) ->
        Socket.close s
