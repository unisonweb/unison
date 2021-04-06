```unison
type Foo = Foo
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Foo

```
```ucm
  ☝️  The namespace .myLib is empty.

.myLib> debug.file

  type Foo#568rsi7o3g

.myLib> add

  ⍟ I've added these definitions:
  
    type Foo

.myLib> push /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-one-type-192da04d6362dbe8/repo.git

  Done.

```

-------
```ucm
  ☝️  The namespace .yourLib is empty.

.yourLib> pull /private/var/folders/gg/lqv4nxmx0nv3_35tg9_8c15r0000gn/T/git-simple-one-type-192da04d6362dbe8/repo.git

  Here's what's changed in the current namespace after the
  merge:
  
  Added definitions:
  
    1. type Foo
    2. Foo.Foo : ()
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

```
```unison
> Foo.Foo
```

```ucm

  ✅
  
  scratch.u changed.
  
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

    1 | > Foo.Foo
          ⧩
          ()

```
