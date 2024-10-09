# find api

``` unison
rachel.filesystem.x = 42
ross.httpClient.y = 43
joey.httpServer.z = 44
joey.yaml.zz = 45
```

``` ucm
scratch/main> add
```

``` api
-- Namespace segment prefix search
GET /api/projects/scratch/branches/main/find?query=http

-- Namespace segment suffix search
GET /api/projects/scratch/branches/main/find?query=Server

-- Substring search
GET /api/projects/scratch/branches/main/find?query=lesys

-- Cross-segment search
GET /api/projects/scratch/branches/main/find?query=joey.http
```
