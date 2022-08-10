# Get Definitions Test

```ucm:hide
.> builtins.mergeio
```

```unison:hide
{{ Documentation }}
nested.names.x = 42
```

```ucm:hide
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

```unison:hide
doctest.thing.doc = {{ The correct docs for the thing }}
doctest.thing = "A thing"
doctest.otherstuff.thing.doc = {{ A doc for a different term with the same name }}
doctest.otherstuff.thing = "A different thing"
```

```ucm:hide
.> add
```

Only docs for the term we request should be returned, even if there are other term docs with the same suffix.

```api
GET /api/getDefinition?names=thing&relativeTo=doctest
```

If we request a doc, the api should return the source, but also the rendered doc should appear in the 'termDocs' list.

```api
GET /api/getDefinition?names=thing.doc&relativeTo=doctest
```
