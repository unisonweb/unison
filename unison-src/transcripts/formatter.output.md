```unison
-- TODO: support formatting docs with {{  }} syntax.
-- For now we just skip formatting any .doc terms.
{{ # Doc
This is a *doc*! 

term link {x}

type link {type   Optional}

}}
x : 
  Nat 
  -> Nat
x y =
    x   =     1 + 1
    x + y
-- Should keep comments after

-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc = {{
# Here's a top-level doc

With a paragraph

Or two
}}
-- After

type Optional   a = More Text 
  | Some 
  | Other   a 
  | None Nat 

ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Optional a
      ability Thing
      explicit.doc : Doc2
      oneLiner     : Doc2
      x            : Nat -> Nat
      x.doc        : Doc2

```
```ucm
.> debug.format

```
```unison:added-by-ucm scratch.u
-- TODO: support formatting docs with {{  }} syntax.
-- For now we just skip formatting any .doc terms.
x.doc =
  {{ # Doc This is a **doc**!
  
    term link {x}
    
    type link {type Optional} }}
x : Nat -> Nat
x y =
  use Nat +
  x = 1 + 1
  x + y
-- Should keep comments after

-- Test for a previous regression that added extra brackets.
oneLiner = {{ one liner }}
-- After

-- Before
explicit.doc =
  {{ # Here's a top-level doc
  
    With a paragraph
    
    Or two }}
-- After

type Optional a = More Text | Some | Other a | None Nat 

ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int
```

