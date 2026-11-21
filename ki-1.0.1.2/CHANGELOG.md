## [1.0.1.2] - July 15, 2024

- Bugfix [#33](https://github.com/awkward-squad/ki/issues/33): A scope could erroneously fail to propagate an exception
  to one of its children.
- Refactor: depend on (rather than inline) `int-supply` package.

## [1.0.1.1] - October 10, 2023

- Compat: support GHC 9.8.1

## [1.0.1.0] - April 3, 2023

- Change [#25](https://github.com/awkward-squad/ki/pull/25): Attempting to fork a thread in a closing scope now acts as
  if it were a child being terminated due to the scope closing. Previously, attempting to fork a thread in a closing
  scope would throw a runtime exception like `error "ki: scope closed"`.
- Change [#27](https://github.com/awkward-squad/ki/pull/27): Calling `awaitAll` on a closed scope now returns `()`
  instead of blocking forever.

## [1.0.0.2] - January 25, 2023

- Bugfix [#20](https://github.com/awkward-squad/ki/pull/20): previously, a child thread could deadlock when attempting
  to propagate an exception to its parent.

## [1.0.0.1] - August 14, 2022

- Compat: support GHC 9.4.1

## [1.0.0] - June 30, 2022

- Breaking: Remove `Context` type, `Ki.Implicit` module, and the ability to soft-cancel a `Scope`.
- Breaking: Remove `Duration` type and its associated API, including `waitFor` and `awaitFor`.
- Breaking: Remove `Ki.Internal` module.
- Breaking: Generalize `async` to `forkTry`.
- Breaking: Generalize `forkWithUnmask` to `forkWith`.
- Breaking: Make `fork_` take an `IO Void` rather than an `IO ()`.
- Breaking: Make `fork` create an unmasked thread, rather than inherit the parent's masking state.
- Breaking: Rename `waitSTM` to `awaitAll` (replacing the old `wait` in `IO`).

- Change: Make `scoped` kill threads in the order they were created.

- Bugfix: Fix small memory leak related to closing a scope.
- Bugfix: Fix subtle bug related to GHC's treatment of deadlocked threads.
- Bugfix: make `async` (now `forkTry`) propagate async exceptions.
- Bugfix: make `scoped` safe to run with asynchronous exceptions masked.
- Bugfix: propagate exceptions to creator of scope, not creator of thread

- Performance: Use atomic fetch-and-add rather than a `TVar` to track internal child thread ids.

## [0.2.0] - December 17, 2020

- Breaking: Remove `ThreadFailed` exception wrapper.
- Breaking: Rename `cancelScope` to `cancel`.

## [0.1.0.1] - November 30, 2020

- Misc: Replace `AtomicCounter` with `Int` to drop the `atomic-primops` dependency.

- Bounds: Lower `cabal-version` from 3.0 to 2.2 because `stack` cannot parse 3.0.

## [0.1.0] - November 11, 2020

- Initial release.
