EasyTest is a simple testing toolkit, meant to replace most uses of QuickCheck, SmallCheck, HUnit, and frameworks like Tasty, etc. Here's an example usage:

```Haskell
module Main where

import EasyTest
import Control.Applicative
import Control.Monad

suite :: Test ()
suite = tests
  [ scope "addition.ex1" $ expect (1 + 1 == 2)
  , scope "addition.ex2" $ expect (2 + 3 == 5)
  , scope "list.reversal" . fork $ do
      -- generate lists from size 0 to 10, of Ints in (0,43)
      -- shorthand: listsOf [0..10] (int' 0 43)
      ns <- [0..10] `forM` \n -> replicateM n (int' 0 43)
      ns `forM_` \ns -> expect (reverse (reverse ns) == ns)
  -- equivalent to `scope "addition.ex3"`
  , scope "addition" . scope "ex3" $ expect (3 + 3 == 6)
  , scope "always passes" $ do
      note "I'm running this test, even though it always passes!"
      ok -- like `pure ()`, but records a success result
  , scope "failing test" $ crash "oh noes!!" ]

-- NB: `run suite` would run all tests, but we only run
-- tests whose scopes are prefixed by "addition"
main = runOnly "addition" suite
```

This generates the output:

```
Randomness seed for this run is 5104092164859451056
Raw test output to follow ...
------------------------------------------------------------
OK addition.ex1
OK addition.ex2
OK addition.ex3
------------------------------------------------------------
✅  3 tests passed, no failures! 👍 🎉
```

The idea here is to write tests with ordinary Haskell code, with control flow explicit and under programmer control. Tests are values of type `Test a`, and `Test` forms a monad with access to:

* repeatable randomness (the `random` and `random'` functions for random and bounded random values, or handy specialized `int`, `int'`, `double`, `double'`, etc)
* I/O (via `liftIO` or `EasyTest.io`, which is an alias for `liftIO`)
* failure (via `crash`, which yields a stack trace, or `fail`, which does not)
* logging (via `note`, `noteScoped`, or `note'`)
* hierarchically-named subcomputations which can be switched on and off (in the above code, notice that only the tests scoped under `"addition"` are run, and we could do `run` instead of `runOnly` if we wanted to run the whole suite)
* parallelism (note the `fork` which runs that subtree of the test suite in a parallel thread).
* conjunction of tests via `MonadPlus` (the `<|>` operation runs both tests, even if the first test fails, and the `tests` function used above is just `msum`).

Using any or all of these capabilities, you assemble `Test` values into a "test suite" (just another `Test` value) using ordinary Haskell code, not framework magic. Notice that to generate a list of random values, we just `replicateM` and `forM` as usual. If this gets tedious... we can factor this logic out into helper functions! For instance:

```Haskell
listOf :: Int -> Test a -> Test [a]
listOf = replicateM

listsOf :: [Int] -> Test a -> Test [[a]]
listsOf sizes gen = sizes `forM` \n -> listOf n gen

ex :: Test ()
ex = do
  ns <- listsOf [0..100] int
  ns `forM_` \ns -> expect (reverse (reverse ns) == ns)
```

This library is opinionated and might not be for everyone. If you're curious about any of the design decisions made, see [my rationale](#rationale) for writing it.

### User guide

The simplest tests are `ok`, `crash`, and `expect`:

```Haskell
-- Record a success
ok :: Test ()

-- Record a failure
crash :: String -> Test a

-- Record a success if `True`, otherwise record a failure
expect :: Bool -> Test ()
```

NB: `fail` is equivalent to `crash`, but doesn't provide a stack trace on failure.

We can lift I/O into `Test` using `io` (or `liftIO`, but I always forget where to import that from):

```
io :: IO a -> Test a
```

`Test` is also a `Monad`. Note that `return` and `pure` do not record a result. Use `ok`, `expect`, or `crash` for that purpose.

We often want to label tests so we can see when they succeed or fail. For that we use `scope`:

```
-- | Label a test. Can be nested. A `'.'` is placed between nested
-- scopes, so `scope "foo" . scope "bar"` is equivalent to `scope "foo.bar"`
scope :: String -> Test a -> Test a
```

Here's an example usage, putting all these primitives together:

```Haskell
module Main where

import EasyTest (ok, scope, crash, expect, run)

suite :: Test ()
suite = do
  ok
  scope "test-crash" $ crash "oh noes!"
  expect (1 + 1 == 2)

main = run suite
```

This example is _sequencing_ the `ok`, `crash`, and `expect` monadically, so the test halts at the first failure. The output is:

```
Randomness seed for this run is 1830293182471192517
Raw test output to follow ...
------------------------------------------------------------
test-crash FAILURE oh noes! CallStack (from HasCallStack):
  crash, called at /Users/pchiusano/code/easytest/tests/Suite.hs:10:24 in main:Main
OK
FAILED test-crash
------------------------------------------------------------


  1 passed
  1 FAILED (failed scopes below)
    "test-crash"

  To rerun with same random seed:

    EasyTest.rerun 1830293182471192517
    EasyTest.rerunOnly 1830293182471192517 "test-crash"


------------------------------------------------------------
❌
```

In the output (which is streamed to the console), we get a stack trace pointing to the line where `crash` was called (`..tests/Suite.hs:10:24`), information about failing tests, and instructions for rerunning the tests with an identical random seed (in this case, there's no randomness, so `rerun` would work fine, but if our test generated random data, we might want to rerun with the exact same random numbers).

The last line of the output always indicates success or failure of the overall suite... and information about any failing tests is _immediately_ above that. You should NEVER have to scroll through a bunch of test output just to find out which tests actually failed! Also, the streaming output always has `OK` or `FAILED` as the _leftmost_ text for ease of scanning.

If you try running a test suite that has no results recorded (like if you have a typo in a call to `runOnly`, or you forget to use `ok` or `expect` to record a test result), you'll see a warning like this:

```
😶  hmm ... no test results recorded
Tip: use `ok`, `expect`, or `crash` to record results
Tip: if running via `runOnly` or `rerunOnly`, check for typos
```

The various `run` functions (`run`, `runOnly`, `rerun`, and `rerunOnly`) all exit the process with a nonzero status in the event of a failure, so they can be used for continuous integration or test running tools that key off the process exit code to determine whether the suite succeeded or failed. For instance, here's the relevant portion of a typical cabal file:

```
-- Preferred way to run EasyTest-based test suite
executable runtests
  main-is:        NameOfYourTestSuite.hs
  ghc-options:    -w -threaded -rtsopts -with-rtsopts=-N -v0
  hs-source-dirs: tests
  other-modules:
  build-depends:
    base,
    easytest

-- I really have no idea why you'd ever use this, unless you
-- really feel the need to run your tests via cabal's "test runner"
-- which "conveniently" hides all output unless you pass it some
-- random flag I never remember
test-suite tests
  type:           exitcode-stdio-1.0
  main-is:        NameOfYourTestSuite.hs
  ghc-options:    -w -threaded -rtsopts -with-rtsopts=-N -v0
  hs-source-dirs: tests
  other-modules:
  build-depends:
    base,
    easytest
```

For tests that are logically separate, we usually combine them into a suite using `tests` (which is just `msum`), as in:

```Haskell
suite = tests
  [ scope "ex1" $ expect (1 + 1 == 2)
  , scope "ex2" $ expect (2 + 2 == 4) ]

-- equivalently
suite =
  (scope "ex1" $ expect (1 + 1 == 2)) <|>
  (scope "ex2" $ expect (2 + 2 == 4))
```

Importantly, each branch of a `<|>` or `tests` gets its own copy of the randomness source, so even when branches of the test suite are switched on or off, the randomness received by a branch is the same. This is important for being able to quickly iterate on a test failure!

Sometimes, tests take a while to run and we want to make use of parallelism. For that, use `EasyTest.fork` or `fork'`:

```Haskell
-- | Run a test in a separate thread, not blocking for its result.
fork :: Test a -> Test ()

-- | Run a test in a separate thread, not blocking for its result, but
-- return a future which can be used to block on the result.
fork' :: Test a -> Test (Test a)
```

Note: There's no "framework global" parallelism configuration setting.

We often want to generate random data for testing purposes:

```Haskell
reverseTest :: Test ()
reverseTest = scope "list reversal" $ do
  nums <- listsOf [0..100] (int' 0 99)
  nums `forM_` \nums -> expect (reverse (reverse nums) == nums)
```

Tip: generate your test cases in order of increasing size. If you get a failure, your test case is closer to "minimal".

The above code generates lists of sizes `0` through `100`, consisting of `Int` values in the range `0` through `99`. `int' :: Int -> Int -> Test Int`, and there are analogous functions for `Double`, `Word`, etc. The most general functions are:

```Haskell
random :: Random a => Test a
random' :: Random a => a -> a -> Test a
```

The functions `int`, `char`, `bool`, `double`, etc are just specialized aliases for `random`, and `int'`, `char'`, etc are just aliases for `random'`. The aliases are sometimes useful in situations where use of the generic `random` or `random'` would require type annotations.

If our list reversal test failed, we might use `runOnly "list reversal"` or `rerunOnly <randomseed> "list reversal"` to rerun just that subtree of the test suite, and we might add some additional diagnostics to see what was going on:

```Haskell
reverseTest :: Test ()
reverseTest = scope "list reversal" $ do
  nums <- listsOf [0..100] (int' 0 99)
  nums `forM_` \nums -> do
    note $ "nums: " ++ show nums
    let r = reverse (reverse nums)
    note $ "reverse (reverse nums): " ++ show r
    expect (r == nums)
```

The idea is that these sorts of detailed diagnostics are added lazily (and temporarily) to find and fix failing tests. You can also add diagnostics via `io (putStrLn "blah")`, but if you have tests running in parallel this can sometimes get confusing.

That's it! Just use ordinary monadic code to generate any testing data and to run your tests.

### <a id="rationale">Why?

Here's some of my thinking in the design of this library:

* Testing should uncomplicated, minimal friction, and ideally: FUN. If I have to think too much or remember arbitrary framework magic, I get irritated.
* A lot of testing frameworks are weirdly optimized for adding lots of diagnostic information up front, as if whatever diagnostic information you happen to think to capture will be exactly what is needed to fix whatever bugs your tests reveal. In my experience this is almost never the case, so EasyTest takes the opposite approach: be EXTREMELY LAZY about adding diagnostics and labeling subexpressions, but make it trivial to reproduce failing tests without running your entire suite. If a test fails, you can easily rerun just that test, with the exact same random seed, and add whatever diagnostics or print statements you need to track down what's wrong. And EasyTest helpfully tells you how to do this rerunning whenever your tests fail, because otherwise I'd never remember. (Again: keep the friction LOW!)
* Another reason not to add diagnostics up front: you avoid needing to remember two different versions of every function or operator (the one you use in your regular code, and the one you use with your testing "framework" to supply diagnostics). HUnit has operators named `(@=?)`, `(~?=)`, and a bunch of others for asserting equality with diagnostics on failure. QuickCheck has `(.&&.)` and `(.||.)`. Just... no.
* HUnit, QuickCheck, SmallCheck, Tasty, and whatever else are frameworks that hide control flow from the programmer and make some forms of control flow difficult or impossible to specify (for instance, you can't do I/O in your regular QuickCheck tests... unless you use `Test.QuickCheck.Monadic`, which has yet another API you have to learn!). In contrast, EasyTest is just a single data type with a monadic API and a few helper functions. You assemble your tests using ordinary monadic code, and there is never any magic. Want to abstract over something? _Write a regular function._ Need to generate some testing data? Write regular functions.
* "How do I modify the number of generated test cases for QuickCheck for just one of my properties?" Or control the maximum size for these `Gen` and `Arbitrary` types? Some arbitrary "configuration setting" that you have to look up every time. No thanks!
* Seriously, global configuration settings are evil! I want fine-grained control over the amount of parallelism, test case sizes, and so on. And if I find I'm repeating myself a lot... I'll _introduce a regular Haskell variable or function!_. DOWN WITH FRAMEWORKS AND THEIR DAMN CONFIGURATION SETTINGS!!
* Most of the functionality of QuickCheck is overkill anyway! There's no need for `Arbitrary` instances (explicit generation is totally fine, and even preferred in most cases), `Coarbitrary` (cute, but not useful when the HOF you are testing is parametric), or shrinking (just generate your test cases in increasing sizes, and your first failure will be the smallest!).

I hope that you enjoy writing your tests with this library!
