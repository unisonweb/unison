# find api

```unison
rachel.filesystem.x = 42
ross.httpClient.y = 43
joey.httpServer.z = 44
joey.yaml.zz = 45
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      joey.httpServer.z   : ##Nat
      joey.yaml.zz        : ##Nat
      rachel.filesystem.x : ##Nat
      ross.httpClient.y   : ##Nat

```
```ucm
scratch/main> add

  ⍟ I've added these definitions:
  
    joey.httpServer.z   : ##Nat
    joey.yaml.zz        : ##Nat
    rachel.filesystem.x : ##Nat
    ross.httpClient.y   : ##Nat

```
```api
--  Namespace segment prefix search
GET /api/projects/scratch/branches/main/find?query=http

```

```api
--  Namespace segment prefix search
GET /api/projects/scratch/branches/main/find?query=http
--  Namespace segment suffix search
GET /api/projects/scratch/branches/main/find?query=Server
--  Substring search
GET /api/projects/scratch/branches/main/find?query=lesys
--  Cross-segment search
GET /api/projects/scratch/branches/main/find?query=joey.http
```


🛑

The transcript failed due to an error in the stanza above. The error is:

Error decoding response from /api/projects/scratch/branches/main/find?query=http: Error in $: Failed reading: not a valid json value at 'QueryparameterrootBranchisrequired'
