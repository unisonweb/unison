
## An example scenario that surfaces an 'ambiguous metadata' error.

```unison:hide
foo.doc = [: a :] 
boo.doc = [: b :]
x = 1
```

```ucm:hide:all
.> add
```

```ucm:error
.> merge foo boo
.> link boo.doc x
```