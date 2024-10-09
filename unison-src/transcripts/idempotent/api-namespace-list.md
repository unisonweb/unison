# Namespace list api

``` ucm :hide
scratch/main> builtins.mergeio
```

``` unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{ I'm a readme! }}
```

``` ucm
scratch/main> add
```

``` api
GET /api/projects/scratch/branches/main/list?namespace=nested.names

GET /api/projects/scratch/branches/main/list?namespace=names&relativeTo=nested
```
