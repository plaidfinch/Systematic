module Systematic.Backend.Real.Sockets where

import Systematic.Language
import Systematic.Enumerator

import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet      as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch

import Data.Function
import Data.Monoid
import Data.Functor

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Prelude hiding (log)

newtype Sockets m a
  = Sockets (EnumeratorT Int m a)
  deriving newtype (Functor, Applicative, Monad,
                    MonadIO, MonadThrow, MonadCatch)

sockets :: MonadIO m => Sockets m a -> m a
sockets (Sockets action) =
  runEnumeratorT 0 action

-- An actual socket, attached to a mutable buffer
data RealSocket f t (mode :: Mode) where
  RealSocket
    :: S.Family f
    => { socketIdentity    :: Int
       , socketTransport   :: Transport t
       , socketHandle      :: S.Socket f t S.Default
       } -> RealSocket f t mode

instance SocketInfo RealSocket where
  socketId (RealSocket{socketIdentity}) = socketIdentity

-- How to initialize a real buffered socket
setupSocket
  :: forall f t mode. S.Family f
  => Int
  -> Transport t
  -> AddressType f
  -> IO (RealSocket f t mode)
setupSocket uniqueId transport addressType =
  withTransportType transport $ do
    socket <- S.socket @f @t @S.Default
    S.setSocketOption socket (S.ReuseAddress True)
    case addressType of
      IPv6 -> S.setSocketOption socket (S.V6Only False) :: IO ()
      _    -> mempty :: IO ()
    return $ wrapSocket uniqueId transport socket

requestSize :: Int
requestSize = 128

-- Wrap an existing real socket in a buffer
wrapSocket
  :: S.Family f
  => Int
  -> Transport t
  -> S.Socket f t S.Default
  -> RealSocket f t mode
wrapSocket uniqueId transport socket =
  RealSocket
    { socketIdentity    = uniqueId
    , socketTransport   = transport
    , socketHandle      = socket }

instance MonadIO m => HasSockets (Sockets m) where
  type Socket (Sockets m) = RealSocket

  connect transport addressType address port =
    Sockets $ do
      uniqueId <- next
      withAddressFamily addressType $ liftIO $ do
        socket@RealSocket{socketHandle = s} <-
          setupSocket uniqueId transport addressType
        S.connect s (actualAddress addressType address port)
        return socket

  listen (addressType :: AddressType f) (Port port) =
    Sockets $ do
      uniqueId <- next
      withAddressFamily addressType $ liftIO $ do
        socket@RealSocket{socketHandle = s} <-
          setupSocket uniqueId TCP addressType
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

  accept RealSocket{socketTransport, socketHandle = s} =
    Sockets $ do
      uniqueId <- next
      liftIO $ do
        (s', _) <- S.accept s
        return $ wrapSocket uniqueId socketTransport s'

  send RealSocket{socketTransport, socketHandle = s} string =
    liftIO $ case socketTransport of
      TCP -> void $ S.sendAll s string S.msgNoSignal
      UDP -> void $ S.send    s string S.msgNoSignal

  receive RealSocket{socketHandle = s} =
    liftIO $ S.receive s requestSize S.msgNoSignal

  close RealSocket{socketHandle = s} =
    liftIO $ S.close s


-- Resolving actual addresses
actualAddress :: AddressType f -> Address f -> Port -> S.SocketAddress f
actualAddress IPv4 tuple (Port p) =
  S.SocketAddressInet
    (S.inetAddressFromTuple tuple)
    (fromIntegral p)
actualAddress IPv6 tuple (Port p) =
  S.SocketAddressInet6
    (S.inet6AddressFromTuple tuple)
    (fromIntegral p) 0 0

-- Proving the existence of instances
withAddressFamily :: AddressType f -> (S.Family f => r) -> r
withAddressFamily IPv4 k = k
withAddressFamily IPv6 k = k

withTransportType :: Transport t -> (S.Type t => r) -> r
withTransportType TCP k = k
withTransportType UDP k = k

-- Utility functions

maybeSplit :: Char -> ByteString -> Maybe (ByteString, ByteString)
maybeSplit char string =
  if BS.length suffix > 0
  then Just (prefix, BS.drop 1 suffix)
  else Nothing
  where
    (prefix, suffix) = BS.break (== char) string


-- Boilerplate

instance MonadTrans Sockets where
  lift = Sockets . lift

instance HasThreads m => HasThreads (Sockets m) where
  type ThreadId (Sockets m) = ThreadId m
  fork (Sockets process) = do
    e <- Sockets enumerator
    tid <- lift . fork . void $
             withEnumerator e process
    return tid
  kill = lift . kill

instance HasLog m => HasLog (Sockets m) where
  log = lift . log

instance HasMemory m => HasMemory (Sockets m) where
  type Ref (Sockets m) = Ref m
  newRef    = lift .  newRef
  readRef   = lift .  readRef
  writeRef  = lift .: writeRef

  type Var (Sockets m) = Var m
  newVar      = lift .  newVar
  newEmptyVar = lift    newEmptyVar
  takeVar     = lift .  takeVar
  putVar      = lift .: putVar

  type Channel (Sockets m) = Channel m
  newChan   = lift    newChan
  readChan  = lift .  readChan
  writeChan = lift .: writeChan
