# A Design Overview of the Systematic Framework

Systematic is a Haskell library for implementing distributed systems, designed with an emphasis on pedagogy and testing. Specifically, Systematic is an _embedded domain-specific language_ with multiple back-ends. The design goals of this language are that it should be

- **high-level**, making it easy for a student of distributed systems to understand algorithms written in the language,
- **helpful**, providing useful compile-time errors when a programmer incorrectly uses its API, and
- **testable**, allowing distributed algorithms to be empirically verified against declarative specifications

We deliberately favor simplicity and testability over completeness and performance, nevertheless attempting to provide reasonable flexibility and speed at the same time.

## What is a Systematic system?

A distributed system in the Systematic framework consists of one or more _programs_. These programs may be executed on the same or different nodes. In the case of testing, we will run all the programs in a system on a single physical machine, simulating the network between them. When running these programs as a distributed system in the real world, they may be operating on one or more different physical machines, connected by the real network.

Systematic provides a system of modular back-ends, which give the end user flexibility in selecting an appropriate way to execute their program. Rather than providing a series of monolithic back-ends, Systematic provides a selection of different "layers" for building a back-end. The user may select a different back-end from any number of choices to handle each layer of the language. Presently, Systematic has the following layers:

- **Threading**: forking new threads and killing other threads
- **Logging**: logging messages of arbitrary type to trace execution
- **Memory**: mutable references and FIFO channels
- **Synchronization**: blocking "vars" (generalization of semaphores)
- **Network Sockets**: a very simplified and type-safe POSIX socket API

Because all of these interfaces map to operations built into the native Haskell runtime and standard library, it is always possible to run a Systematic program as if it were just an ordinary Haskell program, and doing so incurs almost no runtime cost.

However, a user might instead choose to run a program using a mock user-level scheduler for threading, a local file for logging, Haskell's native memory and synchronization, and a simulated network for handling sockets. Most such combinations of back-end layers are possible, and the Systematic's type system will prevent invalid back-end layer combinations at compile time.

## Design considerations for the domain-specific language

The Systematic framework decouples the description of a network-connected program from its actual implementation by means of an embedded domain specific language (eDSL) in Haskell. The advantage of an _embedded_ domain-specific language is that programs written inside it to make use of all the features of the "host" language (in this case, Haskell). While this allows the eDSL to gain tremendous expressiveness for free, it also comes with a disadvantage: programs in the embedded language must use the host language's type system---which may or may not provide the appropriate constructs for ensuring the type safety of the embedded language. Haskell's type system is sufficient to express all the type-safety guarantees we would like to ensure for the Systematic language, but the type error messages it produces are certainly less than optimal, and future work is needed to improve the usability of this component. On the whole, though, the eDSL approach seems to have more benefits than drawbacks for this project.

### The Expression Problem for eDSLs

For this project, it is necessary that the Systematic eDSL be extensible in two distinct ways:

1. It should be easy to add new language constructs, and
2. It should be easy to add new back-end interpreters.

It's relatively easy to design a framework which permits one kind of extensibility but not the other. The difficulty is when both are desired at the same time. In the simplest approaches, adding a new language construct requires altering every interpreter for the language, and adding a new interpreter requires implementing every language construct: in other words, as such a language grows, the effort required by the programmer becomes quadratic.

This problem is known as "the expression problem," and there are many solutions to it in the literature, each with their own advantages and drawbacks. We explored several of those, eventually settling on one common to the Haskell standard library: monad transformer typeclasses. This is frequently called the "MTL style" after the "Monad Transformer Library" which pioneered the approach (and which is still widely used in the Haskell ecosystem).

In the MTL style of eDSL, each layer of the language interface (threading, logging, memory, etc.) is described by its own interface in Haskell, represented by a _typeclass_.

Typeclasses are similar to traditional object-oriented interfaces, with the distinction that a datatype's implementation of an interface is decoupled from the definition of that datatype and the definition of the interface: neither the author of the interface nor the datatype needs to know in advance about the other in order for some third party to implement the interface for the type. Such implementations are called _typeclass instances_. Below is an example of a simple typeclass, and an instance implementing it for a datatype:

```
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

### MTL-Style Language Interpreters

In the MTL style, we use typeclasses to describe what parts of the language a particular backend implements. For example, we describe the abstract interface to mutable memory cells (`Ref`s) like this (simplified from actual version):

```
class HasMemory backend where
  type Ref backend
  newRef   :: a -> backend (Ref backend a)
  readRef  :: Ref backend a -> backend a
  writeRef :: Ref backend a -> a -> backend ()
```

This says that any backend implementing the `HasMemory` portion of the Systematic language needs to specify a particular type `Ref` which it will use to implement references, as well as define the three functions `newRef`, `readRef`, and `writeRef`, all of which operate over that chosen type. Different backends may make different choices about what type they use to represent references---one may use Haskell's native mutable references, whereas another may use a purely-functional simulated heap. Analogously, we define interfaces for the other features of our language.

Any program written in the Systematic language, though, is not permitted to depend on any particular backend---by guaranteeing that such programs use _only_ the operations defined in these interfaces, we ensure that they can be executed on _any_ back-end.

A Systematic program must be given the _Haskell_ type `Program`, and the Haskell (host) language's type system will prevent Systematic programs from depending on implementation details of a particular back-end.

```
type Program a
  = forall backend.
      ( HasThreads backend
      , HasLog     backend
      , HasMemory  backend
      , HasSync    backend
      , HasSockets backend
      ) => backend a
```

In the above type definition, the "`forall`" indicates that any Systematic program which can be given the type `Program` uses only the operations defined in the listed back-end typeclasses (`HasThreads`, `HasLog`, etc.).
