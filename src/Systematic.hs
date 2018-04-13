{-# language StrictData #-}

module Systematic
  ( TCP
  , UDP
  , IPv4
  , IPv6
  , Process
  , Mode(..)
  , Transport(..)
  , Address(..)
  , Port(..)
  , SocketID(..)
  , SocketInfo(..)
  , LocalEvent(..)
  , connect
  , listen
  , accept
  , send
  , receive
  , close
  , logMessage
  , runSystem
  , localhost
  , logAsHaskell
  , prettySysCall
  ) where

import Data.ByteString ( ByteString )
import Data.Binary
import Control.Monad.Operational as O
import GHC.Generics
import Data.Functor
import Data.IORef
import Type.Reflection

import qualified System.Socket                  as Socket
import qualified System.Socket.Family.Inet      as Socket
import qualified System.Socket.Family.Inet6     as Socket
import qualified System.Socket.Type.Stream      as Socket
import qualified System.Socket.Type.Datagram    as Socket
import qualified System.Socket.Protocol.Default as Socket

type TCP  = Socket.Stream
type UDP  = Socket.Datagram
type IPv4 = Socket.Inet
type IPv6 = Socket.Inet6

newtype Process socket a
  = Process (Program (Network socket) a)
  deriving newtype (Functor, Applicative, Monad)

data Mode
  = Listening
  | Connected
  deriving (Eq, Ord, Show)

data Transport t where
  TCP :: Transport TCP
  UDP :: Transport UDP

deriving instance Eq (Transport t)
deriving instance Ord (Transport t)
deriving instance Show (Transport t)

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
  LogMessage
    :: (Typeable message, Show message)
    => message
    -> Network socket ()

connect
  :: Transport t
  -> Address f
  -> Port
  -> Process socket (socket f t Connected)
connect transport address port =
  Process $ O.singleton (Connect transport address port)

listen
  :: Transport t
  -> Address f
  -> Port
  -> Process socket (socket f t Listening)
listen transport address port =
  Process $ O.singleton (Listen transport address port)

accept
  :: socket f t Listening
  -> Process socket (socket f t Connected)
accept socket =
  Process $ O.singleton (Accept socket)

send
  :: socket f t Connected
  -> ByteString
  -> Process socket ()
send socket bytestring =
  Process $ O.singleton (Send socket bytestring)

receive
  :: socket f t Connected
  -> Int
  -> Process socket ByteString
receive socket bufferSize =
  Process $ O.singleton (Receive socket bufferSize)

close
  :: socket f t mode
  -> Process socket ()
close socket =
  Process $ O.singleton (Close socket)

logMessage
  :: (Typeable message, Show message)
  => message
  -> Process socket ()
logMessage message =
  Process $ O.singleton (LogMessage message)

-- Implementation against actual network sockets

data Address f where
  IPv4 :: Word8 -> Word8 -> Word8 -> Word8
       -> Address IPv4
  IPv6 :: Word16 -> Word16 -> Word16 -> Word16
       -> Word16 -> Word16 -> Word16 -> Word16
       -> Address IPv6

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
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Binary)

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

class SocketInfo socket where
  socketID        :: socket f t mode -> SocketID
  socketTransport :: socket f t mode -> Transport t

instance SocketInfo ActualSocket where
  socketID (ASocket i _ _) = i
  socketTransport (ASocket _ t _) = t

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
    --Socket.setSocketOption s (Socket.V6Only False)  -- TODO: Only for IPv6
    return (ASocket i transport s)

data LocalEvent socket where
  LocalEvent :: Network socket a -> a -> LocalEvent socket

runSystem
  :: (forall socket. SocketInfo socket => LocalEvent socket -> IO ())
  -> (forall socket. SocketInfo socket => Process socket a)
  -> IO a
runSystem logEvent (Process program) = do
  socketIDs <- newIDStream (SocketID 0)
  interpretWithMonad (evalLogging socketIDs) program
  where
    evalLogging, eval
      :: IDStream SocketID -> Network ActualSocket a -> IO a

    -- TODO: How to print syscall before executing, then bound result after?
    evalLogging socketIDs syscall = do
      result <- eval socketIDs syscall
      logEvent (LocalEvent syscall result)
      return result

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

      LogMessage _ ->
        mempty

localhost :: Address IPv4
localhost = IPv4 127 0 0 1

logAsHaskell
  :: SocketInfo socket
  => (Integer -> String)
  -> (String -> IO ())
  -> (forall a. (Typeable a, Show a) => a -> IO ())
  -> LocalEvent socket
  -> IO ()
logAsHaskell socketName logNetwork forwardLog = \case
  LocalEvent syscall result -> do
    maybe mempty logNetwork
      (prettySysCall socketName syscall result)
    case syscall of
      LogMessage message -> forwardLog message
      _ -> mempty

prettySysCall
  :: SocketInfo socket
  => (Integer -> String)
  -> Network socket a
  -> a
  -> Maybe String
prettySysCall socketName syscall result =
  let prettied :: String =
        case syscall of
          Connect transport address port ->
            concat [ nameSocket result
                  , " <- connect "
                  , show transport
                  , " ("
                  , show address
                  , ") ("
                  , show port
                  , ")"
                  ]
          Listen transport address port ->
            concat [ nameSocket result
                  , " <- listen "
                  , show transport
                  , " ("
                  , show address
                  , ") ("
                  , show port
                  , ")"
                  ]
          Accept socket ->
            concat [ nameSocket result
                  , " <- accept "
                  , nameSocket socket
                  ]
          Send socket string ->
            concat [ "send "
                  , nameSocket socket
                  , " "
                  , show string
                  ]
          Receive socket bufferSize ->
            concat [ show result
                  , " <- receive "
                  , nameSocket socket
                  , " "
                  , show bufferSize
                  ]
          Close socket ->
            concat [ "close "
                  , nameSocket socket
                  ]
          LogMessage message ->
            concat [ "logMessage @"
                   , showsPrec 11 (typeOf message) ""
                   , " "
                   , showsPrec 11 message ""
                   ]
  in if prettied == "" then Nothing else Just prettied
  where
    nameSocket :: SocketInfo socket => socket f t mode -> String
    nameSocket (socketID -> SocketID i) = socketName i
