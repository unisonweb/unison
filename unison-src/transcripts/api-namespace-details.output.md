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
GET /api/projects/scratch/branches/main/namespaces/nested.names

```



🛑

The transcript failed due to an error in the stanza above. The error is:

Error decoding response from /api/projects/scratch/branches/main/namespaces/nested.names: Error in $: Failed reading: not a valid json value at 'QueryparameterrootBranchisrequired'
