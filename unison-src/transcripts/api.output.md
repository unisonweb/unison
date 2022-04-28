# API Test

```unison
x = 42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    x : Nat

```
```api
GET /api/getDefinition?names=x
{
    "termDefinitions": {
        "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8": {
            "signature": [
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                }
            ],
            "termDocs": [],
            "termDefinition": {
                "contents": [
                    {
                        "annotation": {
                            "contents": "x",
                            "tag": "HashQualifier"
                        },
                        "segment": "x"
                    },
                    {
                        "annotation": {
                            "tag": "TypeAscriptionColon"
                        },
                        "segment": " :"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": {
                            "contents": "x",
                            "tag": "HashQualifier"
                        },
                        "segment": "x"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "42"
                    }
                ],
                "tag": "UserObject"
            },
            "termNames": [
                "x"
            ],
            "defnTermTag": null,
            "bestTermName": "x"
        }
    },
    "typeDefinitions": {},
    "missingDefinitions": []
}```