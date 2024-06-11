# Tests for `move.namespace`

```ucm:hide
scratch/main happy> builtins.merge
scratch/main history> builtins.merge
scratch/main existing> builtins.merge
```

## Happy path

Create a namespace and add some history to it

```unison
a.termInA = 1
unique type a.T = T
```

```ucm
scratch/main happy> add
```

```unison
a.termInA = 2
unique type a.T = T1 | T2
```

```ucm
scratch/main happy> update
```

Should be able to move the namespace, including its types, terms, and sub-namespaces.

```ucm
scratch/main happy> move.namespace a b
scratch/main happy> ls b
scratch/main happy> history b
```


## Namespace history


Create some namespaces and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm
scratch/main history> add
```

```unison
a.termInA = 2
b.termInB = 11
```

```ucm
scratch/main history> update
```

Deleting a namespace should not leave behind any history,
if we move another to that location we expect the history to simply be the history
of the moved namespace.

```ucm
scratch/main history> delete.namespace b
scratch/main history> move.namespace a b
-- Should be the history from 'a'
scratch/main history> history b
-- Should be empty
scratch/main history> history a
```


## Moving over an existing branch

Create some namespace and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm
scratch/main existing> add
```

```unison
a.termInA = 2
b.termInB = 11
```

```ucm
scratch/main existing> update
scratch/main existing> move.namespace a b
```

## Moving the Root

I should be able to move the root into a sub-namespace

```ucm
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
scratch/main> move.namespace .root.at.path.happy .
scratch/main> move.namespace .root.at.path.happy .
scratch/main> ls
scratch/main> history
```


```ucm:error
-- should be empty
scratch/main> ls .root.at.path.happy
scratch/main> history .root.at.path.happy
```
