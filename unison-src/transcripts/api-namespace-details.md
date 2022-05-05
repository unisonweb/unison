# Get Definitions Test

```ucm:hide
.> builtins.mergeio
```

```unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{
Here's a *README*!
}}
```

```ucm
.> add
```

```api
-- Should find names by suffix
GET /api/namespaces/nested.names
```
