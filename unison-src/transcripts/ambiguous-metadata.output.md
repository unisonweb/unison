
## An example scenario that surfaces an 'ambiguous metadata' error.

```unison
foo.doc = [: a :] 
boo.doc = [: b :]
x = 1
```

```ucm
.> merge foo boo

  Here's what's changed in boo after the merge:
  
  New name conflicts:
  
    1. doc#7ivmrc4c8v : #p65rcethk2
       ↓
    2. ┌ doc#7ivmrc4c8v : #p65rcethk2
    3. └ doc#9f3kmo37cv : #p65rcethk2
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> link boo.doc x

  ⚠️
  
  I'm not sure which metadata value you're referring to since
  there are multiple matches:
  
    doc#7ivmrc4c8v
    foo.doc
  
  Tip: Try again and supply one of the above definitions
       explicitly.

```
