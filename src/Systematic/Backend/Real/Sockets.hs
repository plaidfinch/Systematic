module Systematic.Backend.Real.Sockets where

import Systematic.Language
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

import Foreign.C.Types

import Prelude hiding (log)

newtype Sockets m a
  = Sockets (m a)
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadFix
    , HasLog, HasTextLog, HasThreads, HasMemory )

instance MonadTrans Sockets where
  lift = Sockets

sockets :: Sockets m a -> m a
sockets (Sockets action) = action

-- An actual socket, attached to a mutable buffer
data RealSocket f t (mode :: Mode) where
  RealSocket
    :: S.Family f
    => { socketTransport   :: Transport t
       , socketHandle      :: S.Socket f t S.Default
       } -> RealSocket f t mode

-- How to initialize a real buffered socket
setupSocket
  :: forall f t mode. S.Family f
  => Transport t
  -> AddressType f
  -> IO (RealSocket f t mode)
setupSocket transport addressType =
  withTransportType transport $ do
    socket <- S.socket @f @t @S.Default
    S.setSocketOption socket (S.ReuseAddress True)
    case addressType of
      IPv6 -> S.setSocketOption socket (S.V6Only False) :: IO ()
      _    -> mempty :: IO ()
    return $ RealSocket transport socket

requestSize :: Int
requestSize = 128

liftSocketIO :: MonadIO m => IO a -> m a
liftSocketIO =
  liftIO . rethrowPosixError @S.SocketException (coerce @_ @CInt)

instance MonadIO m => HasSockets (Sockets m) where
  type Socket (Sockets m) = RealSocket

  connect transport addressType address port =
    Sockets $ do
      withAddressFamily addressType $ liftSocketIO $ do
        socket@RealSocket{socketHandle = s} <-
          setupSocket transport addressType
        S.connect s (actualAddress addressType address port)
        return socket

  listen (addressType :: AddressType f) (Port port) =
    Sockets $ do
      withAddressFamily addressType $ liftSocketIO $ do
        socket@RealSocket{socketHandle = s} <-
          setupSocket TCP addressType
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
      liftSocketIO $ do
        (s', _) <- S.accept s
        return $ RealSocket socketTransport s'

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
