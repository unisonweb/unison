```unison
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

{{ 
A Doc before a type 
}}
type Optional   a = More Text 
  | Some 
  | Other   a 
  | None Nat 

{{ A doc before an ability }}
ability Thing where
  more  : Nat -> Text -> Nat
  doThing  : Nat -> Int
```

```ucm

  Loading changes detected in scratch.u.

  offset=171:
  unexpected Optional.doc
     31 | {{ 
  

```



🛑

The transcript failed due to an error in the stanza above. The error is:


  offset=171:
  unexpected Optional.doc
     31 | {{ 
  

