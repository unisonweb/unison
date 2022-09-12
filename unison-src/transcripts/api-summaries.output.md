# Definition Summary APIs

```unison
nat : Nat
nat = 42
doc : Doc2
doc = {{ Hello }}
test> mytest = [Test.Result.Ok "ok"]
func : Text -> Text
func x = x ++ "hello"

funcWithLongType : Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text -> Text
funcWithLongType a b c d e f g h = a ++ b ++ c ++ d ++ e ++ f ++ g ++ h

structural type Thing = This Nat | That
```

## Term Summary APIs

```api
--  term
GET /api/definitions/terms/by_name/nat@qkhkl0n238/summary
{
    "fqn": "nat",
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
    "tag": "plain"
}
--  doc
GET /api/definitions/terms/by_name/doc@icfnhas71n/summary
{
    "fqn": "doc",
    "hash": "#icfnhas71n",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "contents": "#ej86si0ur1lsjade71dojr25phk9bbom9rdks6dltolos5tjivakujcriqe02npba53n9gd7tkh8bmv08ttjb9t35lq2ch5heshqcs0",
                    "tag": "TypeReference"
                },
                "segment": "Doc2"
            }
        ],
        "tag": "UserObject"
    },
    "tag": "doc"
}
--  test
GET /api/definitions/terms/by_name/mytest@u17p9803hd/summary
{
    "fqn": "mytest",
    "hash": "#u17p9803hd",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "tag": "DelimiterChar"
                },
                "segment": "["
            },
            {
                "annotation": {
                    "contents": "#aql7qk3iud6vs4cvu43aimopoosgk0fnipibdkc3so13adencmibgfn0u5c01r0adei55nkl3ttsjhl8gbj7tr4gnpj63g64ftbq6s0",
                    "tag": "TypeReference"
                },
                "segment": "Result"
            },
            {
                "annotation": {
                    "tag": "DelimiterChar"
                },
                "segment": "]"
            }
        ],
        "tag": "UserObject"
    },
    "tag": "test"
}
--  function
GET /api/definitions/terms/by_name/func@6ee6j48hk3/summary
{
    "fqn": "func",
    "hash": "#6ee6j48hk3",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "contents": "##Text",
                    "tag": "TypeReference"
                },
                "segment": "Text"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
    "tag": "plain"
}
--  constructor
GET /api/definitions/terms/by_name/Thing.This@altimqs66j@0/summary
{
    "fqn": "Thing.This",
    "hash": "#altimqs66j#0",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "contents": "##Nat",
                    "tag": "TypeReference"
                },
                "segment": "Nat"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#altimqs66j3dh94dpab5pg7j5adjrndq61n803j7fg0v0ohdiut6or66bu1fiongpd45s5euiuo8ru47b928aqv8osln1ikdeg05hq0",
                    "tag": "TypeReference"
                },
                "segment": "Thing"
            }
        ],
        "tag": "UserObject"
    },
    "tag": "data-constructor"
}
--  Long type signature
GET /api/definitions/terms/by_name/funcWithLongType@ieskgcjjvu/summary
{
    "fqn": "funcWithLongType",
    "hash": "#ieskgcjjvu",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "contents": "##Text",
                    "tag": "TypeReference"
                },
                "segment": "Text"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
    "tag": "plain"
}
--  Long type signature with render width
GET /api/definitions/terms/by_name/funcWithLongType@ieskgcjjvu/summary?renderWidth=20
{
    "fqn": "funcWithLongType",
    "hash": "#ieskgcjjvu",
    "summary": {
        "contents": [
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
                    "tag": "TypeOperator"
                },
                "segment": "->"
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
    "tag": "plain"
}
```