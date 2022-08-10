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
                "names.x"
            ]
        }
    },
    "typeDefinitions": {}
}
--  Should find definitions by hash, names should be relative
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
                "names.x"
            ]
        }
    },
    "typeDefinitions": {}
}
--  Should filter out any definitions which aren't in the provided namespace even if the hash matches.
GET /api/getDefinition?names=%23qkhkl0n238&relativeTo=emptypath
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
            "termDocs": [],
            "termNames": [
                ".nested.names.x"
            ]
        }
    },
    "typeDefinitions": {}
}
``````unison
doctest.thing.doc = {{ The correct docs for the thing }}
doctest.thing = "A thing"
doctest.otherstuff.thing.doc = {{ A doc for a different term with the same name }}
doctest.otherstuff.thing = "A different thing"
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      doctest.otherstuff.thing     : Text
      doctest.otherstuff.thing.doc : Doc2
      doctest.thing                : Text
      doctest.thing.doc            : Doc2

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    doctest.otherstuff.thing     : Text
    doctest.otherstuff.thing.doc : Doc2
    doctest.thing                : Text
    doctest.thing.doc            : Doc2

```
Only docs for the term we request should be returned, even if there are other term docs with similar names.

```api
GET /api/getDefinition?names=thing&relativeTo=doctest
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#jksc1s5kud95ro5ivngossullt2oavsd41s3u48bch67jf3gknru5j6hmjslonkd5sdqs8mr8k4rrnef8fodngbg4sm7u6au564ekjg": {
            "bestTermName": "thing",
            "defnTermTag": null,
            "signature": [
                {
                    "annotation": {
                        "contents": "##Text",
                        "tag": "TypeReference"
                    },
                    "segment": "Text"
                }
            ],
            "termDefinition": {
                "contents": [
                    {
                        "annotation": {
                            "contents": "thing",
                            "tag": "HashQualifier"
                        },
                        "segment": "thing"
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
                            "contents": "##Text",
                            "tag": "TypeReference"
                        },
                        "segment": "Text"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": {
                            "contents": "thing",
                            "tag": "HashQualifier"
                        },
                        "segment": "thing"
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
                            "tag": "TextLiteral"
                        },
                        "segment": "\"A thing\""
                    }
                ],
                "tag": "UserObject"
            },
            "termDocs": [
                [
                    "thing.doc",
                    "#t9qfdoiuskj4n9go8cftj1r83s43s3o7sppafm5vr0bq5feieb7ap0cie5ed2qsf9g3ig448vffhnajinq81pnnkila1jp2epa7f26o",
                    {
                        "contents": [
                            {
                                "contents": "The",
                                "tag": "Word"
                            },
                            {
                                "contents": "correct",
                                "tag": "Word"
                            },
                            {
                                "contents": "docs",
                                "tag": "Word"
                            },
                            {
                                "contents": "for",
                                "tag": "Word"
                            },
                            {
                                "contents": "the",
                                "tag": "Word"
                            },
                            {
                                "contents": "thing",
                                "tag": "Word"
                            }
                        ],
                        "tag": "Paragraph"
                    }
                ]
            ],
            "termNames": [
                "thing"
            ]
        }
    },
    "typeDefinitions": {}
}
```