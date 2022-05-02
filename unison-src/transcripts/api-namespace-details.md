# Namespace details api

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
GET /api/namespaces/nested.names
```
