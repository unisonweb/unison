# Behaviour of namespace histories during a merge.

Note: This is a descriptive test meant to capture the current behaviour of 
branch histories during a merge.
It isn't prescriptive about how merges _should_ work with respect to child branches, 
but I think we should at least notice if we change things by accident.


```ucm:hide
.> builtins.merge
```

```unison
parent.top = "top"
parent.child.thing = "parent.child.thing"
```

The child branch has a single history node representing the addition of `parent.child.thing`.

```ucm
.> add
.> history parent.child
```

If we add another thing to the child namespace it should add another history node.

```unison
parent.child.thing2 = "parent.child.thing2"
```


```ucm
.> add
.> history parent.child
```

Now we fork the parent namespace to make some changes.

```ucm
.> fork parent parentfork
```

```unison
parentfork.child.thing3 = "parentfork.child.thing3"
```

```ucm
.> add
.> history parentfork.child
```

Now, if I squash-merge parentfork back into parent, we expect `parentfork.child.thing3` to be added.

```ucm
.> merge.squash parentfork parent
.> history parentfork
```

Notice that with the current behaviour, the history of `parent.child` is completely wiped out.
This doesn't seem desirable.

```ucm
.> history parent.child
```
