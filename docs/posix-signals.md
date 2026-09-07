# POSIX signal subscriptions

`IO.signal` lets Unison programs receive POSIX signal notifications without
running Unison code inside an operating-system signal handler. Terminal
libraries can await SIGWINCH; servers can use SIGHUP to reload configuration
or SIGTERM for graceful shutdown.

The runtime provides two opaque types, `IO.signal.Signal` and
`IO.signal.Subscription`, and four operations:

```unison
IO.signal.available : '{IO} [(Text, IO.signal.Signal)]
IO.signal.subscribe : IO.signal.Signal ->{IO, Exception} IO.signal.Subscription
IO.signal.Subscription.await : IO.signal.Subscription ->{IO, Exception} ()
IO.signal.Subscription.close : IO.signal.Subscription ->{IO, Exception} ()
```

`available` returns supported names such as `SIGHUP` and `SIGWINCH`, together
with platform-correct opaque values. Aliases such as SIGIO and SIGPOLL may
refer to the same signal. Windows returns an empty list. Libraries can layer
named conveniences and scoped cleanup over these primitives.

Subscribe before starting work which can produce a signal. Each subscription
retains a pending notification between calls to `await`. Multiple subscriptions
receive independent notifications; multiple waiters on the *same* subscription
compete for that subscription's notification. Repeated notifications coalesce:
this API does not count signals, guarantee their ordering, or expose siginfo
payloads. Cancelling an `await` leaves the subscription active.

Always close subscriptions, including on exceptions and cancellation. Dropping
a handle does not unsubscribe it. Closing
is idempotent and wakes pending waits with an IO failure. Awaiting a closed
subscription also raises an IO failure. A library should provide a scoped
resource wrapper, using its normal exception and thread-cancellation cleanup.
These primitive subscriptions are process-local resources and cannot be saved
in a codebase or sent to another runtime.

Signal dispositions are process-wide. While there is at least one subscription,
the runtime's notification handler replaces the previous disposition: for
example, SIGHUP becomes a notification instead of terminating the process.
Closing the last subscription restores the previous Haskell handler and the
complete native sigaction, including its flags and mask. This also preserves
handlers installed by native libraries such as curses. Other code must not
replace a signal's handler while subscriptions to that signal are active.

This API supports asynchronous standard signals available on the host. It
excludes SIGKILL and SIGSTOP, synchronous fault/abort signals, GHC's timer
signal, and SIGPIPE (which GHC uses to interrupt blocking foreign calls).
Real-time signals need queued-payload and ordering semantics and are not
included in this notification API. SIGCHLD reports a change; it does not reap
a child or provide its exit status. SIGWINCH reports a resize; the terminal
library still queries the dimensions, for example with `ioctl(TIOCGWINSZ)`.

Implementation: GHC schedules a Haskell action for each delivered signal. That
action broadcasts into STM notification cells. Registration and final cleanup
are serialized; callbacks from an earlier installation cannot reach a later
generation of subscriptions. A small C shim preserves native sigactions, which
`System.Posix.Signals.installHandler` alone cannot fully restore.

Native tests cover delivery, independent subscribers, cancellation, close,
and restoration of Haskell and native handlers. The manual
`unison-src/transcripts-manual/posix-signals.md` transcript checks SIGHUP and
SIGWINCH delivery through the Unison builtins in a disposable UCM process.
