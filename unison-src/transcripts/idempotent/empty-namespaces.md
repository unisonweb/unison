# Empty namespace behaviours

``` unison :hide
mynamespace.x = 1
```

``` ucm :hide
> add

> delete.namespace mynamespace
```

The deleted namespace shouldn't appear in `ls` output.

``` ucm :error
> ls .

  nothing to show
```

``` ucm :error
> find.verbose

  ☝️

  I couldn't find matches in this namespace, searching in
  'lib'...

  😶

  No results. Check your spelling, or try using tab completion
  to supply command arguments.

  `debug.find.global` can be used to search outside the current
  namespace.
```

``` ucm :error
> find mynamespace

  ☝️

  I couldn't find matches in this namespace, searching in
  'lib'...

  😶

  No results. Check your spelling, or try using tab completion
  to supply command arguments.

  `debug.find.global` can be used to search outside the current
  namespace.
```

## history

The history of the namespace should be empty.

``` ucm
> history mynamespace

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #sg60bvjo91 (start of history)
```

Add and then delete a term to add some history to a deleted namespace.

``` unison :hide
deleted.x = 1
stuff.thing = 2
```

``` ucm :hide
> add

> delete.namespace deleted
```

## fork

I should be allowed to fork over a deleted namespace

``` ucm
> fork stuff deleted

  Done.
```

The history from the `deleted` namespace should have been overwritten by the history from `stuff`.

``` ucm
> history stuff

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #q2dq4tsno1 (start of history)

> history deleted

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #q2dq4tsno1 (start of history)
```

## move.namespace

``` unison :hide
moveoverme.x = 1
moveme.y = 2
```

``` ucm :hide
> add
```

I should be able to move a namespace over-top of a deleted namespace.
The history should be that of the moved namespace.

``` ucm
> delete.namespace moveoverme

  Done.

> history moveme

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #c5uisu4kll (start of history)

> move.namespace moveme moveoverme

  Done.

> history moveoverme

  Note: The most recent namespace hash is immediately below this
        message.



  □ 1. #c5uisu4kll (start of history)
```
