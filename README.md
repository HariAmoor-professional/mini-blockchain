# Ouroboros BFT++

This is a toy implementation of IOG's Ouroboros BFT protocol, built as a technical challenge for Anastasia Labs.

Please read through this doc thoroughly before evaluating the code.

## Disclaimer

Technically, this is not a full implementation of the challenge as requested; it doesn't implement pure evaluation of the algorithm through `runPure`.

Unfortunately, there were some technical difficulties on this side (which I go into full detail about in this README), as well as various personal difficulties (which were expressed to the recruiter).

However, I do provide a partial implementation and a small discourse on the subject which might be valuable. Everything else is fully-implemented.

## Usage

I provide examples of the intended usage in `examples/`. You can run these with Nix as follows:

```bash
# Or just `nix run` with no arguments to Nix, since the Ping Pong example is the `defaultPackage` of the flake
nix run .#ping-pong

# The first and only argument is an int stating how many nodes there should be -- the default is 3
nix run .#bft -- 3

# Use Github as a flake registry if you don't wanna clone the whole repo
nix run github:HariAmoor-professional/anastasia-labs-challenge# ...
```

You can also _theoretically_ use this as a package to another test driver through Cabal; I haven't tested this on a separate repo, though, and you'd have to muck around with the types on the client side to pull it off anyway.

## State of the Haskell Ecosystem

In this section, I provide a brief analysis on why Haskell is the way it is and how that impacted my programming decisions.

### Why is everything so complicated?

I was only provided a `Tasks.hs` file, which contained an `HDT.Tasks` module; there was no procedure in place to specify packages, GHC versions, etc.

Therefore, I wrote a small Nix flake and messed around with the module structure for reproducibility. The practices I've implemented form the current industry standard (AFAIK).

See the `Usage` section for more details.

### What is Polysemy and why is it literally _everywhere_?

TL;DR: [Polysemy](https://hackage.haskell.org/package/polysemy) is a library for DSLs; I use it here to implement a DSL around its core free monad.

Techncially, we don't _need_ Polysemy (or even MTL!) to implement free monads. The canonical definition is as given in one of [Serokell's writings](https://serokell.io/blog/introduction-to-free-monads):

```haskell
data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a
```

Theoretically, this is all one needs; however, it's hard to stack effectful programs on top of this, which is why there exists various tooling around free monads.

The most popular free monad tools are Polysemy and [Effectful](https://hackage.haskell.org/package/effectful), the latter being less mature but arguably more performant.

### Seriously, couldn't you (Hari) have done something simpler or less invasive?

Honestly, I probably could have. Unfortunately, like most Haskellers do, I started out with a grand vision before realizing that the ecosystem isn't mature enough to do the whole thing in a reasonable amount of time.

What I really wanted to do was:
1. Provide an IO-based interpreter with STM+TChan as requested
2. Use [delimited continuations](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst) for the pure interpreter
3. Formally verify that the two interpreters are equivalent using [polysemy-check](https://hackage.haskell.org/package/polysemy-check) (Quickcheck for Polysemy)
  a. If I REALLY had time: Write the proof in Agda

I was able to successfully complete (1). The problem I ran into was with (2), where I realized that continuations are relatively new to Haskell and therefore finicky to work with.

## Technical Discourse

Here, I provide more technical details about the limitations I ran into and how I got around them.

### More on Continuations

A continuation is essentially a monad around `callCC :: ((a -> m b) -> m a) -> m a`, which allows you to _wrap_ a specific computation in its surrounding context and execute it conditionally based on future events. This isn't exclusive to Haskell, actually! For example, while the original implementation was in Scheme, and most successors are on similar functional langauges (such as OCaml), there's even a C++ [proposal](https://open-std.org/JTC1/SC22/WG21/docs/papers/2017/p0534r3.pdf) to standardize this!

This is an equivalent building block to `await` and `yield`, which are seen in most asynchronous effect systems (whether pure or otherwise). The theoretical foundation of the idea is in the [Mealy machine](https://en.wikipedia.org/wiki/Mealy_machine), an automaton which is used (in theory) to build concurrent systems consisting of many independent components.

The gist here is that this building block gives you purity-guarantees on concurrent systems for free; you don't need to use it to implement concurrency in IO (although you could use it as an interface over STM!)

Unfortuantely, Polysemy doesn't support continuations on its own; the reason being has to do with how the library _weaves_ effects together to create the final monad. You can see the full details of why in [polysemy-research/polysemy#264](https://github.com/polysemy-research/polysemy/issues/264) and the failed implementation therein.

I could have gotten around this by using the GHC-native implementation, but unfortunately again, this was only released in v.9.6.1 of the compiler. This code is restricted to v.9.2.8 at the moment due to a version constraint on the `unix-compat` library, which we need for IO+STM stuff.

In another effort to get around this, I attempted to use `conduit` as an embedding into Polysemy's free monad. This led to the best results, but the implementation is still unfortunately unfinished.

### What is Conduit and what does it have to do with continuations?

[Conduit](https://hackage.haskell.org/package/conduit) is the package I was planning to use to thread effect streams together; particularly, this, along with [auto](https://hackage.haskell.org/package/auto), would have been helpful to chain together the parameter of `runPure`, which is in `[Agent msg ()]`.

In fact, I provide an interpreter for the Delay+Broadcast+Receive DSL in `HDT.Experimental`, which can be used to create a `runPure` implementation (with sufficient time spent mucking about the docs!)

### What else did you (Hari) have to mess with in order to use this stuff?

The biggest and only change was that the `Agent` datatype, as defined in the given skeleton, wasn't _general_ enough. Particularly, it's of kind `Agent :: Type -> Type`, where what we _really_ want is `Agent :: (Type -> Type) -> Type -> Type`; the extra parameter would be used for a type-level list of allowed effects, which is the standard in Polysemy and Effectful.

Once we have the following type (included in `HDT.Agent`), we're off to the races!

```haskell
data Agent msg :: Effect where
  Delay :: Agent msg m ()
  Broadcast :: msg -> Agent msg m ()
  Receive :: Agent msg m msg
```

With this, you don't even need to define constructors like `delay` and `broadcast` manually; you can auto-generate them with the `TemplateHaskell` snippet `$(makeSem ''Agent)`.

The only other thing which needed change is the type-signature of every function that uses `Agent` as a free monad. Case in point, where one might otherwise use `Agent msg a`, they now have to use `Member (Agent msg) r => Sem r a` (`Sem` being Polysemy's free monad!)

**NOTE:** I emphasize that I didn't change any of the actual _code_ provided by the skeleton. I changed the types of the parameters, but the arity of each function and the calling convention remains the same.