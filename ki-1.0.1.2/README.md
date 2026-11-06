| `ki` | `ki-unlifted` |
| --- | --- |
| [![GitHub CI](https://github.com/awkward-squad/ki/workflows/Haskell-CI/badge.svg)](https://github.com/awkward-squad/ki/actions) | |
| [![Hackage](https://img.shields.io/hackage/v/ki.svg?label=ki&logo=haskell)](https://hackage.haskell.org/package/ki) | [![Hackage](https://img.shields.io/hackage/v/ki-unlifted.svg?label=ki-unlifted&logo=haskell)](https://hackage.haskell.org/package/ki-unlifted) |
| [![Stackage LTS](https://stackage.org/package/ki/badge/lts)](https://www.stackage.org/lts/package/ki) | [![Stackage LTS](https://stackage.org/package/ki-unlifted/badge/lts)](https://www.stackage.org/lts/package/ki-unlifted) |
| [![Stackage Nightly](https://stackage.org/package/ki/badge/nightly)](https://www.stackage.org/nightly/package/ki) | [![Stackage Nightly](https://stackage.org/package/ki-unlifted/badge/nightly)](https://www.stackage.org/nightly/package/ki-unlifted) |
| [![Dependencies](https://img.shields.io/hackage-deps/v/ki)](https://packdeps.haskellers.com/reverse/ki) | [![Dependencies](https://img.shields.io/hackage-deps/v/ki-unlifted)](https://packdeps.haskellers.com/reverse/ki-unlifted) |

# Overview

`ki` is a lightweight structured-concurrency library inspired by many other projects and blog posts:

* [libdill](http://libdill.org/)
* [trio](https://github.com/python-trio/trio)
* [Kotlin coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
* [Notes on structured concurrency, or: Go statement considered harmful](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful)
* [Structured Concurrency in High-level Languages](https://250bpm.com/blog:124)
* [Update on Structured Concurrency](https://250bpm.com/blog:137)
* [Two Approaches to Structured Concurrency](https://250bpm.com/blog:139)
* [libdill: Structured Concurrency for C](https://libdill.org/structured-concurrency.html)

A previous version of `ki` also included a mechanism for soft-cancellation/graceful shutdown, which took inspiration
from:

* [Go Concurrency Patterns: Context](https://blog.golang.org/context)
* [.NET 4 Cancellation Framework](https://devblogs.microsoft.com/pfxteam/net-4-cancellation-framework)
* [Timeouts and cancellation for humans](https://vorpus.org/blog/timeouts-and-cancellation-for-humans)
* [Graceful Shutdown](https://250bpm.com/blog:146)

However, this feature was removed (perhaps temporarily) because the design of the API was unsatisfactory.

# Documentation

[Hackage documentation](https://hackage.haskell.org/package/ki/docs/Ki.html)

# Example: Happy Eyeballs

The [Happy Eyeballs](https://en.wikipedia.org/wiki/Happy_Eyeballs) algorithm is a particularly common example used to
demonstrate the advantages of structured concurrency, because it is simple to describe, but can be surprisingly
difficult to implement.

The problem can be abstractly described as follows: we have a small set of actions to run, each of which can take
arbitrarily long, or fail. Each action is a different way of computing the same value, so we only need to wait for
one action to return successfully. We don't want to run the actions one at a time (because that is likely to take too
long), nor all at once (because that is an improper use of resources). Rather, we will begin executing the first action,
then wait 250 milliseconds, then begin executing the second, and so on, until one returns successfully.

There are of course a number of ways to implement this algorithm. We'll do something non-optimal, but simple. Let's get
the imports out of the way first.

```haskell
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.STM (atomically)
import Data.Function ((&))
import Data.Functor (void)
import Data.List qualified as List
import Data.Maybe (isJust)
import Ki qualified
```

Next, let's define a `staggeredSpawner` helper that implements the majority of the core algorithm: given a list of
actions, spawn them all at 250 millisecond intervals. After all actions are spawned, we block until all of them have
returned.

```haskell
staggeredSpawner :: [IO ()] -> IO ()
staggeredSpawner actions = do
  Ki.scoped \scope -> do
    actions
      & map (\action -> void (Ki.fork scope action))
      & List.intersperse (threadDelay 250_000)
      & sequence_
    atomically (Ki.awaitAll scope)
```

And finally, we wrap this helper with `happyEyeballs`, which accepts a list of actions, and returns when one action
returns successfully, or returns `Nothing` if all actions fail. Note that in a real implementation, we may want to
consider what to do if an action throws an exception. Here, we trust each action to signal failure by returning
`Nothing`.

```haskell
happyEyeballs :: [IO (Maybe a)] -> IO (Maybe a)
happyEyeballs actions = do
  resultVar <- newEmptyMVar

  let worker action = do
        result <- action
        when (isJust result) do
          _ <- tryPutMVar resultVar result
          pure ()

  Ki.scoped \scope -> do
    _ <-
      Ki.fork scope do
        staggeredSpawner (map worker actions)
        tryPutMVar resultVar Nothing
    takeMVar resultVar
```
