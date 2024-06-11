# find api

```unison
rachel.filesystem.x = 42
ross.httpClient.y = 43
joey.httpServer.z = 44
joey.yaml.zz = 45
```

```ucm

  Loading changes detected in scratch.u.

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
scratch/main> add

  ⍟ I've added these definitions:
  
    joey.httpServer.z   : ##Nat
    joey.yaml.zz        : ##Nat
    rachel.filesystem.x : ##Nat
    ross.httpClient.y   : ##Nat

```
```api
--  Namespace segment prefix search
GET /api/non-project-code/find?query=http
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.ross.",
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
            "score": 170
        },
        {
            "contents": {
                "bestFoundTermName": "y",
                "namedTerm": {
                    "termHash": "#emomp74i93h6ps0b5sukke0tci0ooba3f9jk21qm919a7act9u7asani84c0mqbdk4lcjrdvr9olpedp23p6df78r4trqlg0cciadc8",
                    "termName": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.ross.httpClient.y",
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
                        "contents": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.joey.",
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
            "score": 170
        },
        {
            "contents": {
                "bestFoundTermName": "z",
                "namedTerm": {
                    "termHash": "#a84tg4er4kfl9k2p250vp2o1dsp5kmn9a7q8g2bo723qbtbf9sagrl28fa4q0j5f2cv4alsjik6rf487ss646qt95gbm3dd13k7e1fo",
                    "termName": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.joey.httpServer.z",
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
GET /api/non-project-code/find?query=Server
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.joey.http",
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
            "score": 230
        },
        {
            "contents": {
                "bestFoundTermName": "z",
                "namedTerm": {
                    "termHash": "#a84tg4er4kfl9k2p250vp2o1dsp5kmn9a7q8g2bo723qbtbf9sagrl28fa4q0j5f2cv4alsjik6rf487ss646qt95gbm3dd13k7e1fo",
                    "termName": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.joey.httpServer.z",
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
GET /api/non-project-code/find?query=lesys
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.rachel.fi",
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
            "score": 185
        },
        {
            "contents": {
                "bestFoundTermName": "x",
                "namedTerm": {
                    "termHash": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
                    "termName": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.rachel.filesystem.x",
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
GET /api/non-project-code/find?query=joey.http
[
    [
        {
            "result": {
                "segments": [
                    {
                        "contents": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.",
                        "tag": "Gap"
                    },
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
            "score": 333
        },
        {
            "contents": {
                "bestFoundTermName": "z",
                "namedTerm": {
                    "termHash": "#a84tg4er4kfl9k2p250vp2o1dsp5kmn9a7q8g2bo723qbtbf9sagrl28fa4q0j5f2cv4alsjik6rf487ss646qt95gbm3dd13k7e1fo",
                    "termName": "__projects._983a1040_9f2f_4979_bf00_f26b89b303cc.branches._4c0f994b_5a4c_4130_a3ac_7534cb682e97.joey.httpServer.z",
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