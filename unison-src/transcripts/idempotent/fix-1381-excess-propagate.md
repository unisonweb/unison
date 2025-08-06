We were seeing an issue where (it seemed) that every namespace that was visited during a propagate would get a new history node, even when it didn't contain any dependents.

Example:

``` unison :hide
a = "a term"
X.foo = "a namespace"
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Here is an update which should not affect `X`:

``` unison :hide
a = "an update"
```

``` ucm
> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

As of the time of this writing, the history for `X` should be a single node, `#4eeuo5bsfr`;

``` ucm
> history X

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #das1se4g2i (start of history)
```

however, as of release/M1i, we saw an extraneous node appear.  If your `ucm` is fixed, you won't see it below:

``` ucm :error
> history #7nl6ppokhg

  😶

  I don't know of a namespace with that hash.
```
