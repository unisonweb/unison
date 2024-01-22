```ucm:hide
.> builtins.mergeio
```

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
emptyDoc = {{}}

type Optional   a = More Text 
  | Some 
  | Other   a 
  | None Nat 

ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int
```

```ucm
.> debug.format
```
