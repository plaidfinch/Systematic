{-# language StrictData #-}

module Systematic
  ( TCP
  , UDP
  , IPv4
  , IPv6
  , Process
  , Mode(..)
  , Transport(..)
  , HasAddress(..)
  , AddressType(..)
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
import Control.Exception
import Control.Monad.Trans.Maybe

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
    -> AddressType f
    -> Address f
    -> Port
    -> Network socket (socket f t Connected)
  Listen
    :: AddressType f
    -> Port
    -> Network socket (socket f TCP Listening)
  Accept
    :: socket f TCP Listening
    -> Network socket (socket f TCP Connected)
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

-- TODO: Add threads to the DSL
-- Should it log which thread does each line?

-- TODO: What does listen really do? Does it ever make sense to listen from an
-- address other than 0.0.0.0 or 127.0.0.1? If not, then only specify port.

connect
  :: Transport t
  -> AddressType f
  -> Address f
  -> Port
  -> Process socket (socket f t Connected)
connect transport addressType address port =
  Process $ O.singleton (Connect transport addressType address port)

listen
  :: AddressType f
  -> Port
  -> Process socket (socket f TCP Listening)
listen addressType port =
  Process $ O.singleton (Listen addressType port)

accept
  :: socket f TCP Listening
  -> Process socket (socket f TCP Connected)
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

-- TODO: Custom type errors?

-- Implementation against actual network sockets

data AddressType f where
  IPv4 :: AddressType IPv4
  IPv6 :: AddressType IPv6

deriving instance Eq (AddressType f)
deriving instance Ord (AddressType f)
deriving instance Show (AddressType f)

showAddress :: AddressType f -> Address f -> String
showAddress IPv4 = show
showAddress IPv6 = show

class HasAddress f where
  type Address f = r | r -> f
  localhost :: Address f

instance HasAddress IPv4 where
  type Address IPv4
    = (Word8, Word8, Word8, Word8)
  localhost = (127,0,0,1)

instance HasAddress IPv6 where
  type Address IPv6
    = (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
  localhost = (0,0,0,0,0,0,0,1)

actualAddress :: AddressType f -> Address f -> Port -> Socket.SocketAddress f
actualAddress IPv4 tuple (Port p) =
  Socket.SocketAddressInet
    (Socket.inetAddressFromTuple tuple)
    (fromIntegral p)
actualAddress IPv6 tuple (Port p) =
  Socket.SocketAddressInet6
    (Socket.inet6AddressFromTuple tuple)
    (fromIntegral p) 0 0

withAddressFamily :: AddressType f -> (Socket.Family f => r) -> r
withAddressFamily IPv4 k = k
withAddressFamily IPv6 k = k

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

-- TODO: IDStream should not be fixed to a type, but rather hand out strings
-- suffixed with numbers or such, given a particular prefix. Internally, a map
-- from prefix strings to numbers.

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
  -> AddressType f
  -> IO (ActualSocket f t mode)
setupSocket ids transport addressType =
  withTransportType transport $ do
    i <- newID ids
    s <- Socket.socket @f @t @Socket.Default
    Socket.setSocketOption s (Socket.ReuseAddress True)
    case addressType of
      IPv6 -> Socket.setSocketOption s (Socket.V6Only False) :: IO ()
      _    -> mempty :: IO ()
    return (ASocket i transport s)

data LocalEvent socket where
  LocalEvent
    :: Network socket a
    -> Either Socket.SocketException a
    -> LocalEvent socket

runSystem
  :: (forall socket. SocketInfo socket => LocalEvent socket -> IO ())
  -> (forall socket. SocketInfo socket => Process socket a)
  -> IO (Maybe a)
runSystem logEvent (Process program) = do
  socketIDs <- newIDStream (SocketID 0)
  runMaybeT $ interpretWithMonad (evalLogging socketIDs) program
  where
    evalLogging :: IDStream SocketID -> Network ActualSocket a -> MaybeT IO a
    evalLogging socketIDs syscall =
      MaybeT $ catch
        (do result <- eval socketIDs syscall
            logEvent (LocalEvent syscall (Right result))
            return (Just result))
        (\(e :: Socket.SocketException) ->
           do logEvent (LocalEvent syscall (Left e))
              return Nothing)

    eval :: IDStream SocketID -> Network ActualSocket a -> IO a
    eval socketIDs = \case
      Connect transport addressType address port ->
        withAddressFamily addressType $ do
          socket@(ASocket _ _ s) <- setupSocket socketIDs transport addressType
          Socket.connect s (actualAddress addressType address port)
          return socket

      Listen (addressType :: AddressType f) (Port port) ->
        withAddressFamily addressType $ do
          socket@(ASocket _ _ s) <- setupSocket socketIDs TCP addressType
          let socketAddress :: Socket.SocketAddress f
              socketAddress =
                case addressType of
                  IPv4 -> Socket.SocketAddressInet
                    Socket.inetAny (fromIntegral port)
                  IPv6 -> Socket.SocketAddressInet6
                    Socket.inet6Any (fromIntegral port) 0 0
          Socket.bind s socketAddress
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

logAsHaskell
  :: SocketInfo socket
  => (Integer -> String)
  -> (String -> IO ())
  -> (forall a. (Typeable a, Show a) => a -> IO ())
  -> LocalEvent socket
  -> IO ()
logAsHaskell socketName localLog messageLog = \case
  LocalEvent syscall result -> do
    localLog (prettySysCall socketName syscall result)
    case syscall of
      LogMessage message -> messageLog message
      _ -> mempty

prettySysCall
  :: SocketInfo socket
  => (Integer -> String)
  -> Network socket a
  -> Either Socket.SocketException a
  -> String
prettySysCall socketName syscall result =
  call ++ exceptionComment
  where
    call = case syscall of
      Connect transport addressType address port ->
        concat [ maybeBindSocket result
               , "connect "
               , show transport
               , " "
               , show addressType
               , " "
               , showAddress addressType address
               , " ("
               , show port
               , ")"
               ]
      Listen addressType port ->
        concat [ maybeBindSocket result
               , "listen "
               , show addressType
               , " ("
               , show port
               , ")"
               ]
      Accept socket ->
        concat [ maybeBindSocket result
               , "accept "
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

    exceptionComment = case result of
      Right{} -> ""
      Left e -> "\n-- *** Exception: " ++ show e

    maybeBindSocket
      :: SocketInfo socket
      => Either Socket.SocketException (socket f t mode)
      -> String
    maybeBindSocket Left{} = ""
    maybeBindSocket (Right s) = nameSocket s ++ " <- "

    nameSocket :: SocketInfo socket => socket f t mode -> String
    nameSocket (socketID -> SocketID i) = socketName i
