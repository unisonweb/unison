# find api

```unison
rachel.filesystem.x = 42
ross.httpClient.y = 43
joey.httpServer.z = 44
joey.yaml.zz = 45
```

```ucm
.> add
```

```api
-- Namespace segment prefix search
GET /api/find?query=http

-- Namespace segment suffix search
GET /api/find?query=Server

-- Substring search
GET /api/find?query=lesys

-- Cross-segment search
GET /api/find?query=joey.http
```
