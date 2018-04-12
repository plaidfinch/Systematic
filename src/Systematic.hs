module Systematic
  ( Mode(..)
  , Transport(..)
  , HasAddress(..)
  , Network
  , connect
  , listen
  , accept
  , send
  , receive
  , close
  ) where

import Data.ByteString
import Data.Binary
import Control.Monad.Operational as O
import GHC.Generics

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

class HasAddress socket where
  addressOf :: socket f t mode -> Address f
  portOf    :: socket f t mode -> Port

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
  = forall socket. HasAddress socket => Program (Network socket) a

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
  IPv4 :: !Word8 -> !Word8 -> !Word8 -> !Word8
       -> Address Socket.Inet
  IPv6 :: !Word16 -> !Word16 -> !Word16 -> !Word16
       -> !Word16 -> !Word16 -> !Word16 -> !Word16
       -> Address Socket.Inet6

deriving instance Eq (Address f)
deriving instance Ord (Address f)
deriving instance Show (Address f)
-- need a Binary instance for Address

newtype Port
  = Port Word16
  deriving stock (Generic, Eq, Ord)
  deriving anyclass (Binary)
  deriving newtype (Show, Num)

data ActualSocket f t (m :: Mode)
  = ASocket
      !(Address f)
      !Port
      !(Socket.Socket Socket.Inet t Socket.Default)

instance HasAddress ActualSocket where
  addressOf (ASocket a _ _) = a
  portOf    (ASocket _ p _) = p

runSystem :: Process a -> IO a
runSystem program = undefined
