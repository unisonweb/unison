# Namespace details api

```unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{ I'm a readme! }}
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      nested.names.readme : Doc2
      nested.names.x      : Nat
      nested.names.x.doc  : Doc2

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    nested.names.readme : Doc2
    nested.names.x      : Nat
    nested.names.x.doc  : Doc2

```
```api
GET /api/namespaces/nested.names
{
    "fqn": "nested.names",
    "hash": "#oms19b4f9s3c8tb5skeb8jii95ij35n3hdg038pu6rv5b0fikqe4gd7lnu6a1i6aq5tdh2opdo4s0sfrupvk6vfkr9lf0n752gbl8o0",
    "readme": {
        "contents": [
            {
                "contents": "I'm",
                "tag": "Word"
            },
            {
                "contents": "a",
                "tag": "Word"
            },
            {
                "contents": "readme!",
                "tag": "Word"
            }
        ],
        "tag": "Paragraph"
    }
}
```