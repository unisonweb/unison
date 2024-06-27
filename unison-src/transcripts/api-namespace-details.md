# Namespace Details Test

```ucm:hide
scratch/main> builtins.mergeio
```

```unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{
Here's a *README*!
}}
```

```ucm
scratch/main> add
```

```api
-- Should find names by suffix
GET /api/projects/scratch/branches/main/namespaces/nested.names
```
