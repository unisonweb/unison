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

-- Should find definitions by hash.
GET /api/getDefinition?names=%23qkhkl0n238&relativeTo=nested
```
