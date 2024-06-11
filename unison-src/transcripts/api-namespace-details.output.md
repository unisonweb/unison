# Namespace Details Test

```unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{
Here's a *README*!
}}
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      nested.names.readme : Doc2
      nested.names.x      : Nat
      nested.names.x.doc  : Doc2

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    nested.names.readme : Doc2
    nested.names.x      : Nat
    nested.names.x.doc  : Doc2

```
```api
--  Should find names by suffix
GET /api/non-project-code/namespaces/nested.names
{
    "fqn": "nested.names",
    "hash": "#sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg",
    "readme": null
}
```