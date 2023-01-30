# Tests for `move.namespace`

```ucm:hide
.> builtins.mergeio
```

## Happy path

Create a namespace and add some history to it

```unison
a.termInA = 1
unique type a.T = T
```

```ucm
.happy> add
```

```unison
a.termInA = 2
unique type a.T = T1 | T2
```

```ucm
.happy> update
```

Should be able to move the namespace, including its types, terms, and sub-namespaces.

```ucm
.happy> move.namespace a b
.happy> ls b
.happy> history b
```


## Namespace history


Create some namespaces and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm
.history> add
```

```unison
a.termInA = 2
b.termInB = 11
```

```ucm
.history> update
```

Deleting a namespace should not leave behind any history,
if we move another to that location we expect the history to simply be the history
of the moved namespace. 

```ucm
.history> delete.namespace b
.history> move.namespace a b
-- Should be the history from 'a'
.history> history b
-- Should be empty
.history> history a
```


## Moving over an existing branch 

Create some namespace and add some history to them

```unison
a.termInA = 1
b.termInB = 10
```

```ucm
.existing> add
```

```unison
a.termInA = 2
b.termInB = 11
```

```ucm
.existing> update
.existing> move.namespace a b
```

## Moving the Root 

I should be able to move the root into a sub-namespace

```ucm
-- Should request confirmation
.> move.namespace . .root.at.path
.> move.namespace . .root.at.path
.> ls
.> history
```

```ucm
.> ls .root.at.path
.> history .root.at.path
```

I should be able to move a sub namespace _over_ the root.

```ucm
-- Should request confirmation
.> move.namespace .root.at.path.happy .
.> move.namespace .root.at.path.happy .
.> ls
.> history
```


```ucm:error
-- should be empty
.> ls .root.at.path.happy
.> history .root.at.path.happy
```
