# Get Definitions Test

```ucm:hide
scratch/main> builtins.mergeio lib.builtins
```

```unison:hide
nested.names.x.doc = {{ Documentation }}
nested.names.x = 42
```

```ucm:hide
scratch/main> add
```

```api
-- Should NOT find names by suffix
GET /api/projects/scratch/branches/main/getDefinition?names=x

-- Term names should strip relativeTo prefix.
GET /api/projects/scratch/branches/main/getDefinition?names=names.x&relativeTo=nested

-- Should find definitions by hash, names should be relative
GET /api/projects/scratch/branches/main/getDefinition?names=%23qkhkl0n238&relativeTo=nested
```

```unison:hide
doctest.thing.doc = {{ The correct docs for the thing }}
doctest.thing = "A thing"
doctest.thingalias.doc = {{ Docs for the alias, should not be displayed }}
doctest.thingalias = "A thing"
doctest.otherstuff.thing.doc = {{ A doc for a different term with the same name, should not be displayed }}
doctest.otherstuff.thing = "A different thing"
```

```ucm:hide
scratch/main> add
```

Only docs for the term we request should be returned, even if there are other term docs with the same suffix.

```api
GET /api/projects/scratch/branches/main/getDefinition?names=thing&relativeTo=doctest
```

If we request a doc, the api should return the source, but also the rendered doc should appear in the 'termDocs' list.

```api
GET /api/projects/scratch/branches/main/getDefinition?names=thing.doc&relativeTo=doctest
```
