# Watch commands when watching is disabled

In transcript mode (and other non-interactive contexts), file watching is disabled.
All watch-related commands should report that watching is disabled.

``` ucm :error
> watch .

  ⚠️

  I can only watch for changes in interactive sessions, which
  this isn't.
```

``` ucm :error
> unwatch .

  ⚠️

  I can only watch for changes in interactive sessions, which
  this isn't.
```

``` ucm :error
> watches

  ⚠️

  I can only watch for changes in interactive sessions, which
  this isn't.
```
