
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
  
    1. doc#mqm91b53vp : #qpp8bgqet0
       ↓
    2. ┌ doc#a09ch66esd : #qpp8bgqet0
    3. └ doc#mqm91b53vp : #qpp8bgqet0
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

.> link boo.doc x

  ⚠️
  
  I'm not sure which metadata value you're referring to since
  there are multiple matches:
  
    foo.doc
    doc#mqm91b53vp
  
  Tip: Try again and supply one of the above definitions
       explicitly.

```
