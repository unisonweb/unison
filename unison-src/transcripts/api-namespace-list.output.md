# Namespace list api

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
GET /api/list?namespace=nested.names
{
    "namespaceListingChildren": [
        {
            "contents": {
                "termHash": "#ddmmatmmiq",
                "termName": "readme",
                "termTag": "Doc",
                "termType": [
                    {
                        "annotation": {
                            "contents": "#ej86si0ur1",
                            "tag": "HashQualifier"
                        },
                        "segment": "#ej86si0ur1"
                    }
                ]
            },
            "tag": "TermObject"
        },
        {
            "contents": {
                "termHash": "#qkhkl0n238",
                "termName": "x",
                "termTag": "Plain",
                "termType": [
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "HashQualifier"
                        },
                        "segment": "##Nat"
                    }
                ]
            },
            "tag": "TermObject"
        },
        {
            "contents": {
                "namespaceHash": "#n1egracfeljprftoktbjcase2hs4f4p8idbhs5ujipl42agld1810hrq9t7p7ped16aagni2cm1fjcjhho770jh80ipthhmg0cnsur0",
                "namespaceName": "x",
                "namespaceSize": 1
            },
            "tag": "Subnamespace"
        }
    ],
    "namespaceListingFQN": "nested.names",
    "namespaceListingHash": "#oms19b4f9s3c8tb5skeb8jii95ij35n3hdg038pu6rv5b0fikqe4gd7lnu6a1i6aq5tdh2opdo4s0sfrupvk6vfkr9lf0n752gbl8o0"
}
GET /api/list?namespace=names&relativeTo=nested
{
    "namespaceListingChildren": [
        {
            "contents": {
                "termHash": "#ddmmatmmiq",
                "termName": "readme",
                "termTag": "Doc",
                "termType": [
                    {
                        "annotation": {
                            "contents": "#ej86si0ur1",
                            "tag": "HashQualifier"
                        },
                        "segment": "#ej86si0ur1"
                    }
                ]
            },
            "tag": "TermObject"
        },
        {
            "contents": {
                "termHash": "#qkhkl0n238",
                "termName": "x",
                "termTag": "Plain",
                "termType": [
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "HashQualifier"
                        },
                        "segment": "##Nat"
                    }
                ]
            },
            "tag": "TermObject"
        },
        {
            "contents": {
                "namespaceHash": "#n1egracfeljprftoktbjcase2hs4f4p8idbhs5ujipl42agld1810hrq9t7p7ped16aagni2cm1fjcjhho770jh80ipthhmg0cnsur0",
                "namespaceName": "x",
                "namespaceSize": 1
            },
            "tag": "Subnamespace"
        }
    ],
    "namespaceListingFQN": "nested.names",
    "namespaceListingHash": "#oms19b4f9s3c8tb5skeb8jii95ij35n3hdg038pu6rv5b0fikqe4gd7lnu6a1i6aq5tdh2opdo4s0sfrupvk6vfkr9lf0n752gbl8o0"
}
```