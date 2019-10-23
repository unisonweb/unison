# Propagating type edits

```unison
use .builtin

unique type Foo = Foo

fooToInt : Foo -> Int
fooToInt _ = +42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      unique type Foo
      fooToInt : Foo -> builtin.Int
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
  ☝️  The namespace .subpath is empty.

.subpath> add

  ⍟ I've added these definitions:
  
    unique type Foo
    fooToInt : Foo -> .builtin.Int

.subpath> find.verbose

  1. -- #oh6jeikejo05cgtaimmrr7plk5c3lrg6ud63lt6kvf22r5kbgdfupiec0l3u4frmor9hl661o7lp2schtqlffv4t3vn87kq1oi2bfbg
     unique type Foo
     
  2. -- #oh6jeikejo05cgtaimmrr7plk5c3lrg6ud63lt6kvf22r5kbgdfupiec0l3u4frmor9hl661o7lp2schtqlffv4t3vn87kq1oi2bfbg#0
     Foo.Foo : Foo
     
  3. -- #61jbgqnif752uoaq9v046c5fc884d9foamlramo5p8ejqb4et1shs5n0q6g2r5dpig49ocpavvu6pfdsb0526fl333qcqrd2unm188o
     fooToInt : Foo -> .builtin.Int
     
  

.subpath> view fooToInt

  fooToInt : Foo -> .builtin.Int
  fooToInt _ = +42

```
```unison
type Foo = Foo | Bar
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions will replace existing ones of the
      same name and are ok to `update`:
    
      type Foo
   
  Now evaluating any watch expressions (lines starting with
  `>`)... Ctrl+C cancels.

```
```ucm
.subpath> update

  ⍟ I've updated to these definitions:
  
    type Foo

  ✅
  
  No conflicts or edits in progress.

```
