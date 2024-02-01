# Get Definitions Test

```ucm:hide
.nested> builtins.mergeio
```

```unison:hide
{{ Documentation }}
names.x = 42
```

```ucm:hide
.nested> add
```

```api
-- Should NOT find names by suffix
GET /api/non-project-code/getDefinition?names=x

-- Term names should strip relativeTo prefix.
GET /api/non-project-code/getDefinition?names=names.x&relativeTo=nested

-- Should find definitions by hash, names should be relative
GET /api/non-project-code/getDefinition?names=%23qkhkl0n238&relativeTo=nested
```

```ucm:hide
.doctest> builtins.mergeio
```

```unison:hide
thing.doc = {{ The correct docs for the thing }}
thing = "A thing"
thingalias.doc = {{ Docs for the alias, should not be displayed }}
thingalias = "A thing"
otherstuff.thing.doc = {{ A doc for a different term with the same name, should not be displayed }}
otherstuff.thing = "A different thing"
```

```ucm:hide
.doctest> add
```

Only docs for the term we request should be returned, even if there are other term docs with the same suffix.

```api
GET /api/non-project-code/getDefinition?names=thing&relativeTo=doctest
```

If we request a doc, the api should return the source, but also the rendered doc should appear in the 'termDocs' list.

```api
GET /api/non-project-code/getDefinition?names=thing.doc&relativeTo=doctest
```
