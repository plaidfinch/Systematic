module Systematic.Backend.Real.Sockets where

import Systematic.Language
import Systematic.Enumerator
import Systematic.PosixError

import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet      as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Fix

import Data.Function
import Data.Monoid
import Data.Functor
import Data.Coerce

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import Foreign.C.Types

import Prelude hiding (log)

newtype Sockets m a
  = Sockets (EnumeratorT Int m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadFix
    , HasLog, HasTextLog, HasThreads, HasMemory )

instance MonadTrans Sockets where
  lift = Sockets . lift

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

liftSocketIO :: MonadIO m => IO a -> m a
liftSocketIO =
  liftIO . rethrowPosixError @S.SocketException (coerce @_ @CInt)

instance MonadIO m => HasSockets (Sockets m) where
  type Socket (Sockets m) = RealSocket

  connect transport addressType address port =
    Sockets $ do
      uniqueId <- next
      withAddressFamily addressType $ liftSocketIO $ do
        socket@RealSocket{socketHandle = s} <-
          setupSocket uniqueId transport addressType
        S.connect s (actualAddress addressType address port)
        return socket

  listen (addressType :: AddressType f) (Port port) =
    Sockets $ do
      uniqueId <- next
      withAddressFamily addressType $ liftSocketIO $ do
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
      liftSocketIO $ do
        (s', _) <- S.accept s
        return $ wrapSocket uniqueId socketTransport s'

  send RealSocket{socketTransport, socketHandle = s} string =
    liftSocketIO $ case socketTransport of
      TCP -> void $ S.sendAll s string S.msgNoSignal
      UDP -> void $ S.send    s string S.msgNoSignal

  receive RealSocket{socketHandle = s} =
    liftSocketIO $ S.receive s requestSize S.msgNoSignal

  close RealSocket{socketHandle = s} =
    liftSocketIO $ S.close s


-- Utility functions

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

-- Split on a character if it is found
maybeSplit :: Char -> ByteString -> Maybe (ByteString, ByteString)
maybeSplit char string =
  if BS.length suffix > 0
  then Just (prefix, BS.drop 1 suffix)
  else Nothing
  where
    (prefix, suffix) = BS.break (== char) string
