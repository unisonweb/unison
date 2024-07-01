# Get Definitions Test

```unison
nested.names.x.doc = {{ Documentation }}
nested.names.x = 42
```

```api
--  Should NOT find names by suffix
GET /api/projects/scratch/branches/main/getDefinition?names=x

```

```api
--  Should NOT find names by suffix
GET /api/projects/scratch/branches/main/getDefinition?names=x
--  Term names should strip relativeTo prefix.
GET /api/projects/scratch/branches/main/getDefinition?names=names.x&relativeTo=nested
--  Should find definitions by hash, names should be relative
GET /api/projects/scratch/branches/main/getDefinition?names=%23qkhkl0n238&relativeTo=nested
```


🛑

The transcript failed due to an error in the stanza above. The error is:

Error decoding response from /api/projects/scratch/branches/main/getDefinition?names=x: Error in $: Failed reading: not a valid json value at 'QueryparameterrootBranchisrequired'
