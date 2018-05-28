# A Design Overview of the Systematic Framework

Systematic is a Haskell library for implementing distributed systems, designed with an emphasis on pedagogy and testing. Specifically, Systematic is an _embedded domain-specific language_ with multiple backends. The design goals of this language are that it should be

- **high-level**, making it easy for a student of distributed systems to understand algorithms written in the language,
- **helpful**, providing useful compile-time errors when a programmer incorrectly uses its API, and
- **testable**, allowing distributed algorithms to be empirically verified against declarative specifications

We deliberately favor simplicity and testability over completeness and performance, nevertheless attempting to provide reasonable flexibility and speed at the same time.

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

The Systematic framework decouples the description of a network-connected program from its actual implementation by means of an embedded domain specific language (eDSL) in Haskell. The advantage of an _embedded_ domain-specific language is that programs written inside it to make use of all the features of the "host" language (in this case, Haskell). While this allows the eDSL to gain tremendous expressiveness for free, it also comes with a disadvantage: programs in the embedded language must use the host language's type system---which may or may not provide the appropriate constructs for ensuring the type safety of the embedded language. Haskell's type system is sufficient to express all the type-safety guarantees we would like to ensure for the Systematic language, but the type error messages it produces are certainly less than optimal, and future work is needed to improve the usability of this component. On the whole, though, the eDSL approach seems to have more benefits than drawbacks for this project.

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

### Typeclass-based Language Interfaces

In the MTL style, we use typeclasses to describe what parts of the language a particular backend implements. For example, we describe the abstract interface to mutable memory cells (`Ref`s) like this (simplified from actual version):

```haskell
class HasMemory backend where
  type Ref backend
  newRef   :: a -> backend (Ref backend a)
  readRef  :: Ref backend a -> backend a
  writeRef :: Ref backend a -> a -> backend ()
```

This says that any backend implementing the `HasMemory` portion of the Systematic language needs to specify a particular type `Ref` which it will use to implement references, as well as define the three functions `newRef`, `readRef`, and `writeRef`, all of which operate over that chosen type. Different backends may make different choices about what type they use to represent references---one may use Haskell's native mutable references, whereas another may use a purely-functional simulated heap. Analogously, we define interfaces for the other features of our language.

Any program written in the Systematic language, though, is not permitted to depend on any particular backend---by guaranteeing that such programs use _only_ the operations defined in these interfaces, we ensure that they can be executed on _any_ backend.

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

As an example, consider the "real world" backend for memory---that is, the backend which interprets mutable memory operations using the Haskell runtime's native mutable memory operations.

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
