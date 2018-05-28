# A Design Overview of the Systematic Framework

Systematic is a Haskell library for implementing distributed systems, designed with an emphasis on pedagogy and testing. Specifically, Systematic is an _embedded domain-specific language_ with multiple backends. The design goals of this language are that it should be

- **high-level**, making it easy for a student of distributed systems to understand algorithms written in the language,
- **helpful**, providing useful compile-time errors when a programmer incorrectly uses its API, and
- **testable**, allowing distributed algorithms to be empirically verified against declarative specifications

We deliberately favor simplicity and testability over completeness and performance, nevertheless attempting to provide reasonable flexibility and speed at the same time.

While reading, I recommend you start the build process for Systematic, so that when you get to the examples, you're ready to run them.

## Building Systematic

You'll need to install the Haskell build tool `stack`, which can be obtained from <https://docs.haskellstack.org/en/stable/install_and_upgrade>. If you trust the Stack developers enough, you can just run this command:

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

If you're more careful, you can find manual installation links on the above page.

Clone the Systematic repository, then navigate inside of it, and run the command:

```
$ stack build
```

This will probably take a little while. As it's building, read on!

## What is a Systematic system?

A distributed system in the Systematic framework consists of one or more _programs_. These programs may be executed on the same or different nodes. In the case of testing, we will run all the programs in a system on a single physical machine, simulating the network between them. When running these programs as a distributed system in the real world, they may be operating on one or more different physical machines, connected by the real network.

Systematic provides a system of modular backends, which give the end user flexibility in selecting an appropriate way to execute their program. Rather than providing a series of monolithic backends, Systematic provides a selection of different "layers" for building a backend. The user may select a different backend from any number of choices to handle each layer of the language. Presently, Systematic has the following layers:

- **Threading**: forking new threads and killing other threads
- **Logging**: logging messages of arbitrary type to trace execution
- **Memory**: mutable references and FIFO channels
- **Synchronization**: blocking "vars" (generalization of semaphores)
- **Network Sockets**: a very simplified and type-safe POSIX socket API

Because all of these interfaces map to operations built into the native Haskell runtime and standard library, it is always possible to run a Systematic program as if it were just an ordinary Haskell program, and doing so incurs almost no runtime cost.

However, a user might instead choose to run a program using a mock user-level scheduler for threading, a local file for logging, Haskell's native memory and synchronization, and a simulated network for handling sockets. Most such combinations of backend layers are possible, and the Systematic's type system will prevent invalid backend layer combinations at compile time.

## A Framework for Domain-Specific Languages

The Systematic framework decouples the description of a network-connected program from its actual implementation by means of an embedded domain specific language (eDSL) in Haskell. The advantage of an _embedded_ domain-specific language is that programs written inside it to make use of all the features of the "host" language (in this case, Haskell). While this allows the eDSL to gain tremendous expressiveness for free, it also comes with a disadvantage: programs in the embedded language must use the host language's type system&mdash;which may or may not provide the appropriate constructs for ensuring the type safety of the embedded language. Haskell's type system is sufficient to express all the type-safety guarantees we would like to ensure for the Systematic language, but the type error messages it produces are certainly less than optimal, and future work is needed to improve the usability of this component. On the whole, though, the eDSL approach seems to have more benefits than drawbacks for this project.

### The Expression Problem for eDSLs

For this project, it is necessary that the Systematic eDSL be extensible in two distinct ways:

1. It should be easy to add new language constructs, and
2. It should be easy to add new backend interpreters.

It's relatively easy to design a framework which permits one kind of extensibility but not the other. The difficulty is when both are desired at the same time. In the simplest approaches, adding a new language construct requires altering every interpreter for the language, and adding a new interpreter requires implementing every language construct: in other words, as such a language grows, the effort required by the programmer becomes quadratic.

This problem is known as "the expression problem," and there are many solutions to it in the literature, each with their own advantages and drawbacks. We explored several of those, eventually settling on one common to the Haskell standard library: monad transformer typeclasses. This is frequently called the "MTL style" after the "Monad Transformer Library" which pioneered the approach (and which is still widely used in the Haskell ecosystem).

In the MTL style of eDSL, each layer of the language interface (threading, logging, memory, etc.) is described by its own interface in Haskell, represented by a _typeclass_.

Typeclasses are similar to traditional object-oriented interfaces, with the distinction that a datatype's implementation of an interface is decoupled from the definition of that datatype and the definition of the interface: neither the author of the interface nor the datatype needs to know in advance about the other in order for some third party to implement the interface for the type. Such implementations are called _typeclass instances_. Below is an example of a simple typeclass, and an instance implementing it for a datatype:

```haskell
data Employee
  = Employee
      { name    :: String
      , company :: String
      }

class Display a where
  display :: a -> String

instance Display Employee where
  display e =
    name e ++ " works for " ++ company e
```

### Typeclass-Based Language Interfaces

In the MTL style, we use typeclasses to describe what parts of the language a particular backend implements. For example, we describe the abstract interface to mutable memory cells (`Ref`s) like this (simplified from actual version):

```haskell
class HasMemory backend where
  type Ref backend
  newRef   :: a -> backend (Ref backend a)
  readRef  :: Ref backend a -> backend a
  writeRef :: Ref backend a -> a -> backend ()
```

This says that any backend implementing the `HasMemory` portion of the Systematic language needs to specify a particular type `Ref` which it will use to implement references, as well as define the three functions `newRef`, `readRef`, and `writeRef`, all of which operate over that chosen type. Different backends may make different choices about what type they use to represent references&mdash;one may use Haskell's native mutable references, whereas another may use a purely-functional simulated heap. Analogously, we define interfaces for the other features of our language.

Any program written in the Systematic language, though, is not permitted to depend on any particular backend&mdash;by guaranteeing that such programs use _only_ the operations defined in these interfaces, we ensure that they can be executed on _any_ backend.

A Systematic program must be given the _Haskell_ type `Program`, and the Haskell (host) language's type system will prevent Systematic programs from depending on implementation details of a particular backend.

```haskell
type Program a
  = forall backend.
      ( HasThreads backend
      , HasLog     backend
      , HasMemory  backend
      , HasSync    backend
      , HasSockets backend
      ) => backend a
```

In the above type definition, the "`forall`" indicates that any Systematic program which can be given the type `Program` uses only the operations defined in the listed backend typeclasses (`HasThreads`, `HasLog`, etc.).

Adding another set of constructs to the Systematic language requires only that another typeclass be added to the definition of `Program`... and that at least one backend layer be defined which knows how to handle those new constructs.

### Composable Backend Layers

We saw above that the "front end" of the Systematic language consists of a set of typeclass interfaces, which some backend may implement. In order to save programmer effort and increase flexibility, our backend architecture needs to be just as modular as the user view of the language.

A handler for a particular layer of the language consists of a type which wraps an existing (potentially partial) backend interpreter. It defers the interpretation of all but one language interface to the interpreter it wraps, but handles the remaining interface itself.

As an example, consider the "real world" backend for memory&mdash;that is, the backend which interprets mutable memory operations using the Haskell runtime's native mutable memory operations.

```haskell
newtype Memory backend a
  = Memory (backend a)
  deriving
    ( HasLog, HasTextLog, HasThreads, HasSockets, ... )
```

This type declaration indicates that the type `Memory` wraps some other (partial) backend interpreter `backend`, and returning a result of type `a`.

We make extensive use of an extension to Haskell called _generalized newtype deriving_, which allows us to eliminate boilerplate. The `deriving` clause in the datatype declaration instructs the compiler to automatically generate typeclass instances for `Memory` which match those of the underlying `backend`. These operations are lifted (at no runtime cost) over the wrapper type. (The ellipsis in the code above elides other less-relevant classes which we also derive.)

As a result, our `Memory` backend layer only needs to explicitly define how to handle the language interface relevant to it: memory. We just need to manually declare an instance of the `HasMemory` typeclass for our `Memory` wrapper:

```haskell
instance MonadIO m => HasMemory (Memory m) where
  type Ref (Memory m) = IORef
  newRef val       = liftIO (newIORef val)
  readRef ref      = liftIO (readIORef ref)
  writeRef ref val = liftIO (writeIORef ref val)
```

The `IORef` type is Haskell's native type for mutable references. Implementing the `HasMemory` operations for our native memory backend is as simple as mapping between the functions in the `HasMemory` typeclass and their exact analogues in Haskell's `Data.IORef` module. For simulated memory, the wrapper type and associated operations might be significantly more complex, but the same approach applies.

One example of more complex behavior like this is the `LogCommands` wrapper, which can be added to any interpreter stack at any level, and which logs the Systematic language commands which are executed during the course of the program. As a fun trick, the format of the log output is valid Haskell source code.

For instance, the `LogCommands` interpreter wrapper implements `kill` from the `HasThreads` language interface as:

```haskell
instance (HasThreads backend, ...) => HasThreads (LogCommands backend) where
  ...
  kill tid =
    logCommand don't_show
      (lift (kill (nameless tid)) $
      concat [ "kill "
             , nameOf tid
             ]
  ..
```

There's a lot of mechanism in this module for tracking unique names of objects and logging output, but the important thing to notice is that the above implementation of `kill` uses the underlying backend's `kill` method (via a call to `lift`, which runs an action in the underlying backend). Additionally, it performs extra actions: namely, logging the command which was just executed.

The full list of operations in the Systematic language can be found in `src/Systematic/Language.hs`, while the different backend interpreters can be found in `src/Systematic/Backend/`, separated into `Real` and `Mock` backends (which can be freely mixed).

## Socket Programming in Systematic

The most complicated language interface in the Systematic language is the interface for socket programming. This piece of the language is also where we choose to make the most unorthodox simplifications to the underlying computation model, for the purpose of clarity and type-safety.

We build on top of Haskell's `System.Socket` library, simplifying the interface by making the following assumptions:

- Sockets are never re-used after they are opened
- A listening socket is always bound to the wildcard address and a specific port
- TCP and UDP are the only transport layers
- IPv4 and IPv6 are the only addressing schemes
- The size of the write-buffer used by `receive` is implementation-defined and not user-specifiable

Furthermore, we use the type system to enforce some invariants which are already true of the POSIX socket API, but which would cause runtime exceptions if not prevented statically:

- It is only possible to `listen` and `accept` via TCP connections
- It is only possible to `send` or `receive` on a socket which is in the connected state (as opposed to the unbound or listening state)
- All `send` commands transmit the entire string, with no remainder (implementations may translate this to multiple calls to the underlying `SEND(2)` system call)

Enforcing all these invariants leads to a relatively type-safe interface for socket programming. The rest of this section dives into the details of how these guarantees are enforced, and makes use of some relatively heavyweight Haskell type-hackery.

### Enforcing Socket Invariants

We will aggressively apply the typed functional programmer's motto:

> Make illegal states unrepresentable!

We assume that every allocated socket is always going to be used, whether that is to `listen` on it, or to `connect` with it. As such, the user interface has no way to represent a socket which is not currently in either a listening or connected state. We define the `Mode` of a socket:

```haskell
data Mode
  = Listening
  | Connected
```

Sockets will have a _type-indexed_ by this `Mode`, so that it is not possible to use a `Listening` socket in a place where the type system expects a `Connected` one, or vice-versa.

We then define two _singleton types_ to represent the transport layer and address type used by a particular connection. A singleton type is a type whose structure is mirrored in its type index: there is exactly one value of a set of singleton types for each possible type index. Crucially, the type-checker knows this, which means that when the programmer examines a singleton type, more type information is revealed.

```haskell
data Transport t where
  TCP :: Transport TCP
  UDP :: Transport UDP

data AddressType f where
  IPv4 :: AddressType IPv4
  IPv6 :: AddressType IPv6
```

These types will allow us to ensure that the user specifies the correct IP address format for a given connection type. The _type family_ (read: "type-level function") `Address` maps from an address type (like `IPv4`) to the type which represents this variety of addresses (like `(Word8, Word8, Word8, Word8)`).

This means that a function expecting an `AddressType f` and an `Address f` knows that if the `AddressType` is `IPv4`, the `Address` must be a 4-tuple of `Word8`. (In the case of `IPv6`, we represent an address as an 8-tuple of `Word16`.)

With these types in hand, we can describe the socket API exposed to programmers in the Systematic language:

```haskell
class HasSockets backend where

  type Socket backend :: Type -> Type -> Mode -> Type

  connect
    :: Transport t
    -> AddressType f
    -> Address f
    -> Port
    -> backend (Socket backend f t Connected)

  listen
    :: AddressType f
    -> Port
    -> backend (Socket backend f TCP Listening)

  accept
    :: Socket backend f TCP Listening
    -> backend (Socket backend f TCP Connected)

  send
    :: Socket backend f t Connected
    -> ByteString
    -> backend ()

  receive
    :: Socket backend f t Connected
    -> backend ByteString

  close
    :: Socket backend f t mode
    -> backend ()
```

Stepping through this class, a description of each method:

- A backend needs to specify its concrete type of sockets. A `Socket backend` is expected to be indexed by three types, in order: its address type (`IPv4` or `IPv6`), its transport layer (`TCP` or `UDP`), and its `Mode` (`Listening` or `Connected`).
- To `connect`, a new socket is allocated with the given `Transport`, using the given `AddressType`, connected to the given `Address` (of that type), and the given `Port`. When the socket is returned to the user, it is already connected.
- To `listen` as a given `AddressType` on a `Port`, the user must be expecting a `TCP` socket, and one is haded to them which is already in `Listening` mode and bound to that port.
- To `accept` on a `TCP` socket, it must be in `Listening` mode, and the user is given as a result a socket which is already `Connected`.
- To `send` or `receive`, the user gives a `Connected` socket (of any address type and transport), and supplies a bytestring or receives one, respectively.
- The `close` method may be called on any socket in any mode.

Exceptions may still be thrown by calling these methods, and the user is expected to handle them using the standard exception-handling mechanisms built into Haskell. However, many of the most common mistakes with socket programming are eliminated by this typing discipline.

## Putting it Together

Now that we've seen an overview of the composable architecture of the Systematic language, let's see it in action. It can be found in full (with comments) in the file `app/Echo/Main.hs`.

In this example, we implement an echo server. For our purposes, an echo server:

1. listens on a given port,
2. accepts an incoming connection,
3. forks a thread to handle that connection, and
4. loops back to (2) to continue accepting clients.

For each client, the server receives a single line of input from the client and immediately sends it back, looping until the client closes the connection.

We describe this as a Systematic `Program` which returns no interesting value (the unit type `()`):

```haskell
echoServer :: Port -> Program ()
echoServer port = do
  listening <- listen IPv4 port            -- listen on the port via IPv4,
  forever $ do                             -- then in a loop...
    connection <- accept listening         -- accept a new connection, and
    fork (echoWith connection)             -- fork a thread to interact with it
  where
    echoWith connection = do
      maybeLine <- receiveLine connection  -- receive a line over the connection
      case maybeLine of
        Nothing ->                         -- if received nothing, user is done:
          close connection                 -- close the connection and finish;
        Just line -> do                    -- else if user sent a line of input,
          sendLine connection line         -- send it back, and
          echoWith connection              -- loop again, waiting for more
```

Recalling that a `Program` may be run on any backend, we know that this server is defined independently of any particular choice of implementation.

Separately, we can define a stack of interpreter layers to handle the language constructs used by this program. In this case, we'll use the "real" backend layers, with the addition of command-logging via the `LogCommands` wrapper:

```haskell
realRun :: Program a -> IO a
realRun =
  Real.base       -- run using the "real" backend (which includes threading),
  . Real.sockets  -- using real network sockets,
  . Real.memory   -- using real mutable memory to implement buffers & such,
  . logCommands   -- logging all executed commands to stdout
```

In Haskell, the `IO` type represents "impure" computations which need to make use of side effects like mutable memory and concurrency. By assembling an interpreter for a `Program` into `IO`, we can let the Haskell runtime actually execute our server. Note that this is only one of many possible interpreter stacks which enable this.

The remainder of the echo server program in the file is a simple wrapper in ordinary Haskell (outside the Systematic language) which determines the desired port number from the command line arguments given to the executable, and then runs the server using that port number. In the end, it just runs this:

```haskell
realRun (echoServer (Port port))
```

If you started the build of the Systematic project when you started reading, it should be done by now, and you can run this example by navigating to the topmost directory in the Systematic project and running the command:

```
$ stack exec -- echo 8080
```

This terminal window will log the commands executed as the server runs. In another window (or more than one), you can connect to the server using netcat:

```
$ nc localhost 8080
```

As you interact with the server, you should see informative output in the window where you originally executed the server. Once you've closed all the clients (a `^D` will suffice), you can shut down the server by killing it with `^C`.

When the server is killed, your shell may inject some text into its output, and it may be difficult to read the last line of the log. To see exactly what the server logs, you can redirect its output to a file. Note that the server caught the user-interrupt, logged the exception, and quit (this is built into the interpreter stack).
