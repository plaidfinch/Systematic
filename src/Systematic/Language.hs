{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-missing-methods #-}

module Systematic.Language
  ( (.:)
  , TCP
  , UDP
  , IPv4
  , IPv6
  , Mode(..)
  , Transport(..)
  , HasAddress(..)
  , AddressType(..)
  , showAddress
  , Port(..)
  , ThreadInfo(..)
  , HasThreads(..)
  , HasLog(..)
  , RefInfo(..)
  , VarInfo(..)
  , ChannelInfo(..)
  , HasMemory(..)
  , HasSockets(..)
  , SocketInfo(..)
  , sendLine
  , receiveLine
  , HasFiles(..)
  ) where

import Data.ByteString ( ByteString )
import Data.Binary
import Type.Reflection
import Data.Kind
import Data.Monoid

import qualified System.Socket.Family.Inet   ( Inet )
import qualified System.Socket.Family.Inet6  ( Inet6 )
import qualified System.Socket.Type.Stream   ( Stream )
import qualified System.Socket.Type.Datagram ( Datagram )


-- TODO: This shouldn't live here
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g

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

-- Types of things that have unique ids we can query
class ThreadInfo  tid     where threadId  :: tid             -> Int
class RefInfo     ref     where refId     :: ref a           -> Int
class VarInfo     var     where varId     :: var a           -> Int
class ChannelInfo channel where channelId :: channel a       -> Int
class SocketInfo  socket  where socketId  :: socket f t mode -> Int

class (ThreadInfo (ThreadId m), Monad m) => HasThreads m where
  type ThreadId m :: Type
  fork :: m a -> m (ThreadId m)
  kill :: ThreadId m -> m ()

class Monad m => HasLog m where
  log :: (Typeable message, Show message) => message -> m ()

class (RefInfo (Ref m), VarInfo (Var m), ChannelInfo (Channel m), Monad m)
  => HasMemory m where

  type Ref m :: Type -> Type
  newRef    :: a -> m (Ref m a)
  readRef   :: Show a => Ref m a -> m a
  writeRef  :: Show a => Ref m a -> a -> m ()

  type Var m :: Type -> Type
  newVar      :: a -> m (Var m a)
  newEmptyVar :: m (Var m a)
  takeVar     :: Show a => Var m a -> m a
  putVar      :: Show a => Var m a -> a -> m ()

  type Channel m :: Type -> Type
  newChan   :: m (Channel m a)
  readChan  :: Show a => Channel m a -> m a
  writeChan :: Show a => Channel m a -> a -> m ()

class (SocketInfo (Socket m), Monad m) => HasSockets m where
  type Socket m :: Type -> Type -> Mode -> Type
  connect
    :: Transport t -> AddressType f -> Address f -> Port
    -> m (Socket m f t Connected)
  listen       :: AddressType f -> Port -> m (Socket m f TCP Listening)
  accept       :: Socket m f TCP Listening -> m (Socket m f TCP Connected)
  send         :: Socket m f t Connected -> ByteString -> m ()
  receive      :: Socket m f t Connected -> m ByteString
  receiveUntil :: Char -> Socket m f t Connected -> m (Maybe ByteString)
  close        :: Socket m f t mode -> m ()

-- TODO: Is this the right interface?
class Monad m => HasFiles m where
  readFile   :: FilePath -> m ByteString
  writeFile  :: FilePath -> ByteString -> m ()
  appendFile :: FilePath -> ByteString -> m ()
  deleteFile :: FilePath -> m ()

sendLine
  :: HasSockets m
  => Socket m f t Connected -> ByteString
  -> m ()
sendLine socket =
  send socket . (<> "\n")

receiveLine
  :: HasSockets m
  => Socket m f t Connected
  -> m (Maybe ByteString)
receiveLine =
  receiveUntil '\n'