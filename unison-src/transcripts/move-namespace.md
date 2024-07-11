# Tests for `move.namespace`


## Moving the Root

I should be able to move the root into a sub-namespace

```unison:hide
foo = 1
```

```ucm
scratch/main> add
-- Should request confirmation
scratch/main> move.namespace . .root.at.path
scratch/main> move.namespace . .root.at.path
scratch/main> ls
scratch/main> history
```

```ucm
scratch/main> ls .root.at.path
scratch/main> history .root.at.path
```

I should be able to move a sub namespace _over_ the root.

```ucm
-- Should request confirmation
scratch/main> move.namespace .root.at.path .
scratch/main> move.namespace .root.at.path .
scratch/main> ls
scratch/main> history
```


```ucm:error
-- should be empty
scratch/main> ls .root.at.path
scratch/main> history .root.at.path
```

```ucm:hide
scratch/happy> builtins.merge lib.builtins
```

## Happy path

Create a namespace and add some history to it

```unison
a.termInA = 1
unique type a.T = T
```

```ucm
scratch/happy> add
```

```unison
a.termInA = 2
unique type a.T = T1 | T2
```

```ucm
scratch/happy> update
```

Should be able to move the namespace, including its types, terms, and sub-namespaces.

```ucm
scratch/happy> move.namespace a b
scratch/happy> ls b
scratch/happy> history b
```


## Namespace history

```ucm:hide
scratch/history> builtins.merge lib.builtins
```

Create some namespaces and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm
scratch/history> add
```

```unison
a.termInA = 2
b.termInB = 11
```

```ucm
scratch/history> update
```

Deleting a namespace should not leave behind any history,
if we move another to that location we expect the history to simply be the history
of the moved namespace.

```ucm
scratch/history> delete.namespace b
scratch/history> move.namespace a b
-- Should be the history from 'a'
scratch/history> history b
-- Should be empty
scratch/history> history a
```


## Moving over an existing branch

```ucm:hide
scratch/existing> builtins.merge lib.builtins
```

Create some namespace and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm
scratch/existing> add
```

```unison
a.termInA = 2
b.termInB = 11
```

```ucm
scratch/existing> update
scratch/existing> move.namespace a b
```

