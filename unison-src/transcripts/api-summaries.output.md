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
structural type Maybe a = Nothing | Just a

structural ability Stream s where
  send : s -> ()
```

## Term Summary APIs

```api
--  term
GET /api/definitions/terms/qualified/nat@qkhkl0n238/summary
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
    "tag": "Plain"
}
--  doc
GET /api/definitions/terms/qualified/doc@icfnhas71n/summary
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
    "tag": "Doc"
}
--  test
GET /api/definitions/terms/qualified/mytest@u17p9803hd/summary
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
    "tag": "Test"
}
--  function
GET /api/definitions/terms/qualified/func@6ee6j48hk3/summary
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
    "tag": "Plain"
}
--  constructor
GET /api/definitions/terms/qualified/Thing.This@altimqs66j@0/summary
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
    "tag": "DataConstructor"
}
--  Long type signature
GET /api/definitions/terms/qualified/funcWithLongType@ieskgcjjvu/summary
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
    "tag": "Plain"
}
--  Long type signature with render width
GET /api/definitions/terms/qualified/funcWithLongType@ieskgcjjvu/summary?renderWidth=20
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
    "tag": "Plain"
}
--  Builtin Term
GET /api/definitions/terms/qualified/putBytesImpl@@IO.putBytes.impl.v3/summary
{
    "fqn": "putBytesImpl",
    "hash": "##IO.putBytes.impl.v3",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "contents": "##Handle",
                    "tag": "TypeReference"
                },
                "segment": "Handle"
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
                    "contents": "##Bytes",
                    "tag": "TypeReference"
                },
                "segment": "Bytes"
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
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "{"
            },
            {
                "annotation": {
                    "contents": "##IO",
                    "tag": "TypeReference"
                },
                "segment": "IO"
            },
            {
                "annotation": {
                    "tag": "AbilityBraces"
                },
                "segment": "}"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#0o7mf021foma9acqdaibmlh1jidlijq08uf7f5se9tssttqs546pfunjpk6s31mqoq8s2o1natede8hkk6he45l95fibglidikt44v8",
                    "tag": "TypeReference"
                },
                "segment": "Either"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "#r29dja8j9dmjjp45trccchaata8eo1h6d6haar1eai74pq1jt4m7u3ldhlq79f7phfo57eq4bau39vqotl2h63k7ff1m5sj5o9ajuf8",
                    "tag": "TypeReference"
                },
                "segment": "Failure"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": null,
                "segment": "("
            },
            {
                "annotation": null,
                "segment": ")"
            }
        ],
        "tag": "BuiltinObject"
    },
    "tag": "Plain"
}
```## Type Summary APIs

```api
--  data
GET /api/definitions/types/qualified/Thing@altimqs66j/summary
{
    "fqn": "Thing",
    "hash": "#altimqs66j",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "tag": "DataTypeModifier"
                },
                "segment": "structural"
            },
            {
                "annotation": null,
                "segment": " "
            },
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
                    "contents": "Thing#altimqs66j",
                    "tag": "HashQualifier"
                },
                "segment": "Thing"
            },
            {
                "annotation": {
                    "contents": "Thing#altimqs66j",
                    "tag": "HashQualifier"
                },
                "segment": "#altimqs66j"
            }
        ],
        "tag": "UserObject"
    },
    "tag": "Data"
}
--  data with type args
GET /api/definitions/types/qualified/Maybe@nirp5os0q6/summary
{
    "fqn": "Maybe",
    "hash": "#nirp5os0q6",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "tag": "DataTypeModifier"
                },
                "segment": "structural"
            },
            {
                "annotation": null,
                "segment": " "
            },
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
                    "contents": "Maybe#nirp5os0q6",
                    "tag": "HashQualifier"
                },
                "segment": "Maybe"
            },
            {
                "annotation": {
                    "contents": "Maybe#nirp5os0q6",
                    "tag": "HashQualifier"
                },
                "segment": "#nirp5os0q6"
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
            }
        ],
        "tag": "UserObject"
    },
    "tag": "Data"
}
--  ability
GET /api/definitions/types/qualified/Stream@rfi1v9429f/summary
{
    "fqn": "Stream",
    "hash": "#rfi1v9429f",
    "summary": {
        "contents": [
            {
                "annotation": {
                    "tag": "DataTypeModifier"
                },
                "segment": "structural"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "DataTypeKeyword"
                },
                "segment": "ability"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "contents": "Stream#rfi1v9429f",
                    "tag": "HashQualifier"
                },
                "segment": "Stream"
            },
            {
                "annotation": {
                    "contents": "Stream#rfi1v9429f",
                    "tag": "HashQualifier"
                },
                "segment": "#rfi1v9429f"
            },
            {
                "annotation": null,
                "segment": " "
            },
            {
                "annotation": {
                    "tag": "DataTypeParams"
                },
                "segment": "s"
            }
        ],
        "tag": "UserObject"
    },
    "tag": "Ability"
}
--  builtin type
GET /api/definitions/types/qualified/Nat@@Nat/summary
{
    "fqn": "Nat",
    "hash": "##Nat",
    "summary": {
        "contents": [
            {
                "annotation": null,
                "segment": "Nat"
            },
            {
                "annotation": null,
                "segment": "##Nat"
            }
        ],
        "tag": "BuiltinObject"
    },
    "tag": "Data"
}
```