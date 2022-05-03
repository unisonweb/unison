# Get Definitions Test

```unison
{{ Documentation }}
nested.names.x = 42
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      nested.names.x     : Nat
      nested.names.x.doc : Doc2

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    nested.names.x     : Nat
    nested.names.x.doc : Doc2

```
```api
--  Should find names by suffix
GET /api/getDefinition?names=x
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8": {
            "bestTermName": "x",
            "defnTermTag": null,
            "signature": [
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                }
            ],
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
            "termDocs": [
                [
                    "doc",
                    "#ulr9f75rpcrv79d7sfo2ep2tvbntu3e360lfomird2bdpj4bnea230e8o5j0b9our8vggocpa7eck3pus14fcfajlttat1bg71t6rbg",
                    {
                        "contents": [
                            {
                                "contents": "Documentation",
                                "tag": "Word"
                            }
                        ],
                        "tag": "Paragraph"
                    }
                ]
            ],
            "termNames": [
                "nested.names.x"
            ]
        }
    },
    "typeDefinitions": {}
}
--  Term names should strip relativeTo prefix.
GET /api/getDefinition?names=x&relativeTo=nested
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8": {
            "bestTermName": "x",
            "defnTermTag": null,
            "signature": [
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                }
            ],
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
            "termDocs": [
                [
                    "doc",
                    "#ulr9f75rpcrv79d7sfo2ep2tvbntu3e360lfomird2bdpj4bnea230e8o5j0b9our8vggocpa7eck3pus14fcfajlttat1bg71t6rbg",
                    {
                        "contents": [
                            {
                                "contents": "Documentation",
                                "tag": "Word"
                            }
                        ],
                        "tag": "Paragraph"
                    }
                ]
            ],
            "termNames": [
                "names.x",
                "nested.names.x"
            ]
        }
    },
    "typeDefinitions": {}
}
--  Term names should strip relativeTo prefix.
GET /api/getDefinition?names=%23qkhkl0n238&relativeTo=nested
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8": {
            "bestTermName": "x",
            "defnTermTag": null,
            "signature": [
                {
                    "annotation": {
                        "contents": "##Nat",
                        "tag": "TypeReference"
                    },
                    "segment": "Nat"
                }
            ],
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
            "termDocs": [
                [
                    "doc",
                    "#ulr9f75rpcrv79d7sfo2ep2tvbntu3e360lfomird2bdpj4bnea230e8o5j0b9our8vggocpa7eck3pus14fcfajlttat1bg71t6rbg",
                    {
                        "contents": [
                            {
                                "contents": "Documentation",
                                "tag": "Word"
                            }
                        ],
                        "tag": "Paragraph"
                    }
                ]
            ],
            "termNames": [
                "names.x",
                "nested.names.x"
            ]
        }
    },
    "typeDefinitions": {}
}
```