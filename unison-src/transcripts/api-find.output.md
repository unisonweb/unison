# find api

```unison
rachel.filesystem.x = 42
ross.httpClient.y = 43
joey.httpServer.z = 44
joey.yaml.zz = 45
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      joey.httpServer.z   : ##Nat
      joey.yaml.zz        : ##Nat
      rachel.filesystem.x : ##Nat
      ross.httpClient.y   : ##Nat

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    joey.httpServer.z   : ##Nat
    joey.yaml.zz        : ##Nat
    rachel.filesystem.x : ##Nat
    ross.httpClient.y   : ##Nat

```
```api
--  Namespace segment prefix search
GET /api/find?query=http
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "ross.",
                        "tag": "Gap"
                    },
                    {
                        "contents": "http",
                        "tag": "Match"
                    },
                    {
                        "contents": "Client.y",
                        "tag": "Gap"
                    }
                ]
            },
            "score": 156
        },
        {
            "contents": {
                "bestFoundTermName": "y",
                "namedTerm": {
                    "termHash": "#emomp74i93",
                    "termName": "ross.httpClient.y",
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
                }
            },
            "tag": "FoundTermResult"
        }
    ],
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "joey.",
                        "tag": "Gap"
                    },
                    {
                        "contents": "http",
                        "tag": "Match"
                    },
                    {
                        "contents": "Server.z",
                        "tag": "Gap"
                    }
                ]
            },
            "score": 156
        },
        {
            "contents": {
                "bestFoundTermName": "z",
                "namedTerm": {
                    "termHash": "#a84tg4er4k",
                    "termName": "joey.httpServer.z",
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
                }
            },
            "tag": "FoundTermResult"
        }
    ]
]
--  Namespace segment suffix search
GET /api/find?query=Server
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "joey.http",
                        "tag": "Gap"
                    },
                    {
                        "contents": "Server",
                        "tag": "Match"
                    },
                    {
                        "contents": ".z",
                        "tag": "Gap"
                    }
                ]
            },
            "score": 223
        },
        {
            "contents": {
                "bestFoundTermName": "z",
                "namedTerm": {
                    "termHash": "#a84tg4er4k",
                    "termName": "joey.httpServer.z",
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
                }
            },
            "tag": "FoundTermResult"
        }
    ]
]
--  Substring search
GET /api/find?query=lesys
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "rachel.fi",
                        "tag": "Gap"
                    },
                    {
                        "contents": "lesys",
                        "tag": "Match"
                    },
                    {
                        "contents": "tem.x",
                        "tag": "Gap"
                    }
                ]
            },
            "score": 175
        },
        {
            "contents": {
                "bestFoundTermName": "x",
                "namedTerm": {
                    "termHash": "#qkhkl0n238",
                    "termName": "rachel.filesystem.x",
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
                }
            },
            "tag": "FoundTermResult"
        }
    ]
]
--  Cross-segment search
GET /api/find?query=joey.http
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "joey.http",
                        "tag": "Match"
                    },
                    {
                        "contents": "Server.z",
                        "tag": "Gap"
                    }
                ]
            },
            "score": 300
        },
        {
            "contents": {
                "bestFoundTermName": "z",
                "namedTerm": {
                    "termHash": "#a84tg4er4k",
                    "termName": "joey.httpServer.z",
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
                }
            },
            "tag": "FoundTermResult"
        }
    ]
]
```