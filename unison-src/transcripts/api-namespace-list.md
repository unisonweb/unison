# Namespace list api

```ucm:hide
.> builtins.mergeio
```

```unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{ I'm a readme! }}
```

```ucm
.> add
```

```api
GET /api/list?namespace=nested.names

GET /api/list?namespace=names&relativeTo=nested
```
