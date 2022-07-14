# Get Definitions Test

```ucm:hide
.> builtins.mergeio
```

```unison
{{ Documentation }}
nested.names.x = 42
```

```ucm
.> add
```

```api
-- Should find names by suffix
GET /api/getDefinition?names=x

-- Term names should strip relativeTo prefix.
GET /api/getDefinition?names=x&relativeTo=nested

-- Should find definitions by hash, names should be relative
GET /api/getDefinition?names=%23qkhkl0n238&relativeTo=nested

-- Should filter out any definitions which aren't in the provided namespace even if the hash matches.
GET /api/getDefinition?names=%23qkhkl0n238&relativeTo=emptypath
```
