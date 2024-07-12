``` ucm
diffs/main> builtins.merge

  Done.

```
``` unison
term =
  _ = "Here's some text"
  1 + 1

type Type = Type Nat
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type Type
      term : Nat

```
``` ucm
diffs/main> add

  ⍟ I've added these definitions:
  
    type Type
    term : Nat

diffs/main> branch.create new

  Done. I've created the new branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /new`.

```
``` unison
term =
  _ = "Here's some different text"
  1 + 2

type Type a = Type a Text
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These names already exist. You can `update` them to your
      new definition:
    
      type Type a
      term : Nat

```
``` ucm
diffs/new> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

```
Diff terms

``` api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=term&newTerm=term
{
    "diff": {
        "contents": [
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "contents": "term",
                            "tag": "HashQualifier"
                        },
                        "segment": "term"
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
                            "contents": "term",
                            "tag": "HashQualifier"
                        },
                        "segment": "term"
                    },
                    {
                        "annotation": {
                            "tag": "BindingEquals"
                        },
                        "segment": " ="
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "UseKeyword"
                        },
                        "segment": "use "
                    },
                    {
                        "annotation": {
                            "tag": "UsePrefix"
                        },
                        "segment": "Nat"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "UseSuffix"
                        },
                        "segment": "+"
                    },
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "contents": "_",
                            "tag": "HashQualifier"
                        },
                        "segment": "_"
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
                    }
                ]
            },
            {
                "annotation": {
                    "tag": "TextLiteral"
                },
                "diffTag": "segmentChange",
                "fromSegment": "\"Here's some text\"",
                "toSegment": "\"Here's some different text\""
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": null,
                        "segment": "\n"
                    },
                    {
                        "annotation": null,
                        "segment": "  "
                    },
                    {
                        "annotation": {
                            "tag": "NumericLiteral"
                        },
                        "segment": "1"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "##Nat.+",
                            "tag": "TermReference"
                        },
                        "segment": "+"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "annotation": {
                    "tag": "NumericLiteral"
                },
                "diffTag": "segmentChange",
                "fromSegment": "1",
                "toSegment": "2"
            }
        ],
        "tag": "UserObject"
    },
    "diffKind": "diff",
    "newBranchRef": "new",
    "newTerm": {
        "bestTermName": "term",
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
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
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
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "UseKeyword"
                    },
                    "segment": "use "
                },
                {
                    "annotation": {
                        "tag": "UsePrefix"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "_",
                        "tag": "HashQualifier"
                    },
                    "segment": "_"
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
                    "segment": "\"Here's some different text\""
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.+",
                        "tag": "TermReference"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "2"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "term"
        ]
    },
    "oldBranchRef": "main",
    "oldTerm": {
        "bestTermName": "term",
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
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
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
                        "contents": "term",
                        "tag": "HashQualifier"
                    },
                    "segment": "term"
                },
                {
                    "annotation": {
                        "tag": "BindingEquals"
                    },
                    "segment": " ="
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "UseKeyword"
                    },
                    "segment": "use "
                },
                {
                    "annotation": {
                        "tag": "UsePrefix"
                    },
                    "segment": "Nat"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "UseSuffix"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "contents": "_",
                        "tag": "HashQualifier"
                    },
                    "segment": "_"
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
                    "segment": "\"Here's some text\""
                },
                {
                    "annotation": null,
                    "segment": "\n"
                },
                {
                    "annotation": null,
                    "segment": "  "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "##Nat.+",
                        "tag": "TermReference"
                    },
                    "segment": "+"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "NumericLiteral"
                    },
                    "segment": "1"
                }
            ],
            "tag": "UserObject"
        },
        "termDocs": [],
        "termNames": [
            "term"
        ]
    },
    "project": "diffs"
}
```

Diff types

``` api
GET /api/projects/diffs/diff/types?oldBranchRef=main&newBranchRef=new&oldType=Type&newType=Type
{
    "diff": {
        "contents": [
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "DataTypeKeyword"
                        },
                        "segment": "type"
                    },
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "contents": "Type",
                            "tag": "HashQualifier"
                        },
                        "segment": "Type"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    },
                    {
                        "annotation": {
                            "tag": "DataTypeParams"
                        },
                        "segment": "a"
                    }
                ]
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": {
                            "tag": "DelimiterChar"
                        },
                        "segment": " = "
                    }
                ]
            },
            {
                "diffTag": "annotationChange",
                "fromAnnotation": {
                    "contents": "#0tc9e438eurvtevfa6k9pg04qvv66is75hs8iqejkuoaef140g8vvu92hc1ks4gamgc3i1ukgdn0blchp3038l43vffijpsbjh14igo#d0",
                    "tag": "TermReference"
                },
                "segment": "Type",
                "toAnnotation": {
                    "contents": "#mft8mne9i92b6k4m512rn2608rsp6ilq4ejufeof6mbh5aintes4tih1fo93fospmu2t3f0h67uu0mrk2qj75o7k0lj1juefhaidt4g#d0",
                    "tag": "TermReference"
                }
            },
            {
                "diffTag": "both",
                "elements": [
                    {
                        "annotation": null,
                        "segment": " "
                    }
                ]
            },
            {
                "diffTag": "old",
                "elements": [
                    {
                        "annotation": {
                            "contents": "##Nat",
                            "tag": "TypeReference"
                        },
                        "segment": "Nat"
                    }
                ]
            },
            {
                "diffTag": "new",
                "elements": [
                    {
                        "annotation": {
                            "tag": "Var"
                        },
                        "segment": "a"
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
                    }
                ]
            }
        ],
        "tag": "UserObject"
    },
    "diffKind": "diff",
    "newBranchRef": "new",
    "newType": {
        "bestTypeName": "Type",
        "defnTypeTag": "Data",
        "typeDefinition": {
            "contents": [
                {
                    "annotation": {
                        "tag": "DataTypeKeyword"
                    },
                    "segment": "type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "Type",
                        "tag": "HashQualifier"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "DataTypeParams"
                    },
                    "segment": "a"
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": " = "
                },
                {
                    "annotation": {
                        "contents": "#mft8mne9i92b6k4m512rn2608rsp6ilq4ejufeof6mbh5aintes4tih1fo93fospmu2t3f0h67uu0mrk2qj75o7k0lj1juefhaidt4g#d0",
                        "tag": "TermReference"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "tag": "Var"
                    },
                    "segment": "a"
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
                }
            ],
            "tag": "UserObject"
        },
        "typeDocs": [],
        "typeNames": [
            "Type"
        ]
    },
    "oldBranchRef": "main",
    "oldType": {
        "bestTypeName": "Type",
        "defnTypeTag": "Data",
        "typeDefinition": {
            "contents": [
                {
                    "annotation": {
                        "tag": "DataTypeKeyword"
                    },
                    "segment": "type"
                },
                {
                    "annotation": null,
                    "segment": " "
                },
                {
                    "annotation": {
                        "contents": "Type",
                        "tag": "HashQualifier"
                    },
                    "segment": "Type"
                },
                {
                    "annotation": {
                        "tag": "DelimiterChar"
                    },
                    "segment": " = "
                },
                {
                    "annotation": {
                        "contents": "#0tc9e438eurvtevfa6k9pg04qvv66is75hs8iqejkuoaef140g8vvu92hc1ks4gamgc3i1ukgdn0blchp3038l43vffijpsbjh14igo#d0",
                        "tag": "TermReference"
                    },
                    "segment": "Type"
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
                }
            ],
            "tag": "UserObject"
        },
        "typeDocs": [],
        "typeNames": [
            "Type"
        ]
    },
    "project": "diffs"
}
```

