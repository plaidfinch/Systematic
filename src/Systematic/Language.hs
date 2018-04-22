module Systematic.Language
  ( TCP
  , UDP
  , IPv4
  , IPv6
  , Process(..)
  , SysCall(..)
  , Mode(..)
  , Transport(..)
  , HasAddress(..)
  , AddressType(..)
  , showAddress
  , Port(..)
  , ThreadId(..)
  , call
  , connect
  , listen
  , accept
  , send
  , sendLine
  , receive
  , receiveUntil
  , receiveLine
  , close
  , fork
  , logMessage
  ) where

import Data.ByteString ( ByteString )
import Data.Binary
import Control.Monad.Operational as Operational
import Type.Reflection
import Data.Monoid

import qualified System.Socket.Family.Inet   ( Inet )
import qualified System.Socket.Family.Inet6  ( Inet6 )
import qualified System.Socket.Type.Stream   ( Stream )
import qualified System.Socket.Type.Datagram ( Datagram )

-- Type synonyms we export for simplifying the System.Socket interface
type TCP  = System.Socket.Type.Stream.Stream
type UDP  = System.Socket.Type.Datagram.Datagram
type IPv4 = System.Socket.Family.Inet.Inet
type IPv6 = System.Socket.Family.Inet6.Inet6

-- A socket can be in one of two modes
data Mode
  = Listening
  | Connected
  deriving (Eq, Ord, Show)

-- We restrict ourselves to only two kinds of transport
data Transport t where
  TCP :: Transport TCP
  UDP :: Transport UDP

deriving instance Eq (Transport t)
deriving instance Ord (Transport t)
deriving instance Show (Transport t)

-- We only support IPv4 or IPv6
data AddressType f where
  IPv4 :: AddressType IPv4
  IPv6 :: AddressType IPv6

deriving instance Eq (AddressType f)
deriving instance Ord (AddressType f)
deriving instance Show (AddressType f)

showAddress :: AddressType f -> Address f -> String
showAddress IPv4 = show
showAddress IPv6 = show

-- Addresses are indexed by their type, and we define localhost polymorphically

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

-- We connect to ports, which are separate from addresses
newtype Port
  = Port Word16
  deriving stock (Eq, Ord, Show)

-- Threads are numbered sequentially from 0
newtype ThreadId
  = ThreadId Integer
  deriving (Eq, Ord, Show, Enum)

-- Our actual programs will be written in the Process monad
newtype Process socket a
  = Process (Program (SysCall socket) a)
  deriving newtype (Functor, Applicative, Monad)

-- The set of commands available to programs
data SysCall socket a where
  Connect
    :: Transport t
    -> AddressType f
    -> Address f
    -> Port
    -> SysCall socket (socket f t Connected)
  Listen
    :: AddressType f
    -> Port
    -> SysCall socket (socket f TCP Listening)
  Accept
    :: socket f TCP Listening
    -> SysCall socket (socket f TCP Connected)
  Send
    :: socket f t Connected
    -> ByteString
    -> SysCall socket ()
  Receive  -- TODO: put size back in receive
    :: socket f t Connected
    -> SysCall socket ByteString
  ReceiveUntil
    :: Char
    -> socket f t Connected
    -> SysCall socket ByteString
  Close
    :: socket f t mode
    -> SysCall socket ()
  Fork
    :: Process socket ()
    -> SysCall socket ()
  LogMessage
    :: (Typeable message, Show message)
    => message
    -> SysCall socket ()

-- TODO: Add threads to the DSL
-- Should it log which thread does each line?

call :: SysCall socket a -> Process socket a
call = Process . Operational.singleton

connect
  :: Transport t
  -> AddressType f
  -> Address f
  -> Port
  -> Process socket (socket f t Connected)
connect transport addressType address port =
  call (Connect transport addressType address port)

listen
  :: AddressType f
  -> Port
  -> Process socket (socket f TCP Listening)
listen addressType port =
  call (Listen addressType port)

accept
  :: socket f TCP Listening
  -> Process socket (socket f TCP Connected)
accept socket =
  call (Accept socket)

send, sendLine
  :: socket f t Connected
  -> ByteString
  -> Process socket ()
send socket bytestring =
  call (Send socket bytestring)

sendLine socket =
  send socket . (<> "\n")

receive, receiveLine
  :: socket f t Connected
  -> Process socket ByteString
receive socket =
  call (Receive socket)

receiveLine =
  receiveUntil '\n'

receiveUntil
  :: Char
  -> socket f t Connected
  -> Process socket ByteString
receiveUntil char socket =
  call (ReceiveUntil char socket)

close
  :: socket f t mode
  -> Process socket ()
close socket =
  call (Close socket)

fork
  :: Process socket ()
  -> Process socket ()
fork process =
  call (Fork process)

logMessage
  :: (Typeable message, Show message)
  => message
  -> Process socket ()
logMessage message =
  call (LogMessage message)

-- TODO: Custom type errors?
