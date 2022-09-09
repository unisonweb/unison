# Definition Summary APIs

```unison
int = 42
doc = {{ Hello }}
```

## Term Summary APIs

```api
--  Namespace segment prefix search
GET /api/definitions/terms/by_name/int@qkhkl0n238/summary
{
    "fqn": "int",
    "hash": "#qkhkl0n238",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "contents": "##Nat",
                    "tag": "TypeReference"
                },
                "segment": "Nat"
            }
        ],
        "tag": "UserObject"
    },
    "tag": "Plain"
}
```