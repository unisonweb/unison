EasyTest is a simple testing toolkit, meant to replace QuickCheck, SmallCheck, HUnit, Tasty, etc. Here's an example usage:

```Haskell
import EasyTest
import Control.Monad
import Control.Applicative

main = runOnly "addition" $ do
  expect (1 + 1 == 2)
  fork $ do
    ns <- [0..10] `forM` \n -> replicateM n (randomBetween (0 :: Int, 10))
    ns `forM_` \ns -> expect (reverse (reverse ns) == ns)
  scope "addition" $ expect (3 + 3 == 6)
  scope "always passes" $ do
    note "I'm running this test, even though it always passes!"
    ok -- like `pure ()`, but records a success result
  scope "failing test" $ crash "oh noes!!"
```

The library is simple: you just write ordinary Haskell code in the `Test` monad, which has access to:

* random numbers (the `random` and `randomBetween` functions)
* I/O (via `liftIO`)
* failure (via `crash`)
* logging (via `note` or `noteScoped`)
* hierarchically-named subcomputations which can be switched on and off (in the above code, only the `"addition"`-scoped test would be run, and we could do `run` instead if we wanted to run the whole suite)
* parallelism (note the `fork` which runs that subtree of the test suite in a parallel thread).

`Test` is an instance of everything through `MonadPlus` (the `<|>` operation runs both tests, even if the first test fails). You assemble `Test` values into a test suite using ordinary Haskell code, not framework magic. Notice that to generate a list of random values, we just `replicateM` and `forM` as usual. If this gets tedious... we can factor this logic out into helper functions!

This library is opinionated and might not be for everyone. But here's some of my thinking in writing it:

* Testing should uncomplicated, minimal friction, and ideally: FUN.
* A lot of testing frameworks are weirdly optimized for adding lots of diagnostic information up front, as if just whatever diagnostic information you happen to think to capture will magically allow you to fix whatever bugs your tests reveal. EastTest takes the opposite approach: be lazy about adding diagnostics and labeling subexpressions, but make it trivial to reproduce failing tests without running your entire suite. If a test fails, you can easily rerun just that test, with the same random seed, and add whatever diagnostics or print statements you need to track down what's wrong.
* Another reason not to add diagnostics up front: you avoid needing to remember two different versions of every function or operator (the one you use in your regular code, and the one you use with your testing "framework" to supply diagnostics). HUnit has operators named `(@=?)`, `(~?=)`, and a bunch of others for asserting equality with diagnostics on failure. QuickCheck has `(.&&.)` and `(.||.)`. Just... no.
* HUnit, QuickCheck, SmallCheck, Tasty, and whatever else are frameworks that hide control flow from the programmer and make some forms of control flow difficult or impossible to specify (for instance, you can't do I/O in your QuickCheck tests!). In contrast, EasyTest is just a single data type with a monadic API and a few helper functions. You assemble your tests using ordinary monadic code, and there is never any magic. Want to abstract over something? _Write a regular function._ Need to generate some testing data? Write regular functions.
* "How do I modify the number of generated test cases for QuickCheck for just one of my properties?" Or control the maximum size for these `Gen` and `Arbitrary` types? Some arbitrary "configuration setting" that you have to look up every time.
* Global configuration settings are evil. I want fine-grained control over the amount of parallelism, test case sizes, and so on.
* Most of the functionality of QuickCheck is overkill anyway! There's no need for `Arbitrary` instances (explicit generation is totally fine, and even preferred in most cases), `Coarbitrary` (cute, but not useful when the HOF you are testing is parametric), or shrinking (just generate your test cases in increasing sizes, and your first failure will be the smallest).

I hope that you enjoy the library and that it proves useful.
