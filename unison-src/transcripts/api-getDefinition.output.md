# Get Definitions Test

```unison
{{ Documentation }}
nested.names.x = 42
```

```api
--  Should find names by suffix
GET /api/getDefinition?names=x
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8": {
            "bestTermName": "x",
            "defnTermTag": "Plain",
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
            "defnTermTag": "Plain",
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
            "defnTermTag": "Plain",
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
            "defnTermTag": "Plain",
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
doctest.thingalias.doc = {{ Docs for the alias, should not be displayed }}
doctest.thingalias = "A thing"
doctest.otherstuff.thing.doc = {{ A doc for a different term with the same name, should not be displayed }}
doctest.otherstuff.thing = "A different thing"
```

Only docs for the term we request should be returned, even if there are other term docs with the same suffix.

```api
GET /api/getDefinition?names=thing&relativeTo=doctest
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#jksc1s5kud95ro5ivngossullt2oavsd41s3u48bch67jf3gknru5j6hmjslonkd5sdqs8mr8k4rrnef8fodngbg4sm7u6au564ekjg": {
            "bestTermName": "thing",
            "defnTermTag": "Plain",
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
                "thing",
                "thingalias"
            ]
        }
    },
    "typeDefinitions": {}
}
```If we request a doc, the api should return the source, but also the rendered doc should appear in the 'termDocs' list.

```api
GET /api/getDefinition?names=thing.doc&relativeTo=doctest
{
    "missingDefinitions": [],
    "termDefinitions": {
        "#t9qfdoiuskj4n9go8cftj1r83s43s3o7sppafm5vr0bq5feieb7ap0cie5ed2qsf9g3ig448vffhnajinq81pnnkila1jp2epa7f26o": {
            "bestTermName": "thing.doc",
            "defnTermTag": "Doc",
            "signature": [
                {
                    "annotation": {
                        "contents": "#ej86si0ur1lsjade71dojr25phk9bbom9rdks6dltolos5tjivakujcriqe02npba53n9gd7tkh8bmv08ttjb9t35lq2ch5heshqcs0",
                        "tag": "TypeReference"
                    },
                    "segment": "Doc2"
                }
            ],
            "termDefinition": {
                "contents": [
                    {
                        "annotation": {
                            "contents": "thing.doc",
                            "tag": "HashQualifier"
                        },
                        "segment": "thing.doc"
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
                            "contents": "#ej86si0ur1lsjade71dojr25phk9bbom9rdks6dltolos5tjivakujcriqe02npba53n9gd7tkh8bmv08ttjb9t35lq2ch5heshqcs0",
                            "tag": "TypeReference"
                        },
                        "segment": "Doc2"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": {
                            "contents": "thing.doc",
                            "tag": "HashQualifier"
                        },
                        "segment": "thing.doc"
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
                            "tag": "DocDelimiter"
                        },
                        "segment": "{{"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "The"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "correct"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "docs"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "for"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "the"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": null,
                        "segment": "thing"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "DocDelimiter"
                        },
                        "segment": "}}"
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
                "thing.doc"
            ]
        }
    },
    "typeDefinitions": {}
}
```