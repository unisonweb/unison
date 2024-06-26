# Namespace list api

```unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{ I'm a readme! }}
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
GET /api/non-project-code/list?namespace=nested.names
{
    "namespaceListingChildren": [],
    "namespaceListingFQN": "nested.names",
    "namespaceListingHash": "#sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg"
}
GET /api/non-project-code/list?namespace=names&relativeTo=nested
{
    "namespaceListingChildren": [],
    "namespaceListingFQN": "nested.names",
    "namespaceListingHash": "#sg60bvjo91fsoo7pkh9gejbn0qgc95vra87ap6l5d35ri0lkaudl7bs12d71sf3fh6p23teemuor7mk1i9n567m50ibakcghjec5ajg"
}
```