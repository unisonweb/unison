``` ucm :hide
scratch/main> builtins.merge
```

``` unison :hide
type MyType = A | B

type AnotherType = C | D

myVal = A

myNum = 1

mySum = myNum + 2

myCase = match myVal with
  A -> myNum
  B -> 2
```

``` ucm :hide
scratch/main> update
```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependencies?name=MyType
RESPONSE:
  {
      "results": []
  }

```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependencies?name=myVal
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "MyType",
                  "hash": "#0qbc2dfom7m4pputtdojo849g2mp5kkr00kvsvjktb07tcmo1jql53bg73bqiib35vja4a7059rcet0raf7jsh4d8vg5582ibinpqj8",
                  "summary": {
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
                                  "contents": "MyType",
                                  "tag": "HashQualifier"
                              },
                              "segment": "MyType"
                          }
                      ],
                      "tag": "UserObject"
                  },
                  "tag": "Data"
              },
              "fqn": "MyType",
              "kind": "type",
              "projectRef": "scratch"
          }
      ]
  }

```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependencies?name=myNum
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "builtin.Nat",
                  "hash": "##Nat",
                  "summary": {
                      "contents": [
                          {
                              "annotation": null,
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "BuiltinObject"
                  },
                  "tag": "Data"
              },
              "fqn": "builtin.Nat",
              "kind": "type",
              "projectRef": "scratch"
          }
      ]
  }

```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependencies?name=mySum
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "builtin.Nat.+",
                  "hash": "##Nat.+",
                  "summary": {
                      "contents": [
                          {
                              "annotation": {
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
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
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
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
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "BuiltinObject"
                  },
                  "tag": "Plain"
              },
              "fqn": "builtin.Nat.+",
              "kind": "term",
              "projectRef": "scratch"
          },
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "myNum",
                  "hash": "#gjmq673r1vrurfotlnirv7vutdhm6sa3s02em5g22kk606mv6duvv8be402dv79312i4a0onepq5bo7citsodvq2g720nttj0ee9p0g",
                  "summary": {
                      "contents": [
                          {
                              "annotation": {
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "UserObject"
                  },
                  "tag": "Plain"
              },
              "fqn": "myNum",
              "kind": "term",
              "projectRef": "scratch"
          },
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "builtin.Nat",
                  "hash": "##Nat",
                  "summary": {
                      "contents": [
                          {
                              "annotation": null,
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "BuiltinObject"
                  },
                  "tag": "Data"
              },
              "fqn": "builtin.Nat",
              "kind": "type",
              "projectRef": "scratch"
          }
      ]
  }

```

Can also get dependencies by a hash-only:

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependencies?name=@soiu5q8htcd
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "builtin.Nat.+",
                  "hash": "##Nat.+",
                  "summary": {
                      "contents": [
                          {
                              "annotation": {
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
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
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
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
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "BuiltinObject"
                  },
                  "tag": "Plain"
              },
              "fqn": "builtin.Nat.+",
              "kind": "term",
              "projectRef": "scratch"
          },
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "myNum",
                  "hash": "#gjmq673r1vrurfotlnirv7vutdhm6sa3s02em5g22kk606mv6duvv8be402dv79312i4a0onepq5bo7citsodvq2g720nttj0ee9p0g",
                  "summary": {
                      "contents": [
                          {
                              "annotation": {
                                  "contents": "##Nat",
                                  "tag": "TypeReference"
                              },
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "UserObject"
                  },
                  "tag": "Plain"
              },
              "fqn": "myNum",
              "kind": "term",
              "projectRef": "scratch"
          },
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "builtin.Nat",
                  "hash": "##Nat",
                  "summary": {
                      "contents": [
                          {
                              "annotation": null,
                              "segment": "builtin.Nat"
                          }
                      ],
                      "tag": "BuiltinObject"
                  },
                  "tag": "Data"
              },
              "fqn": "builtin.Nat",
              "kind": "type",
              "projectRef": "scratch"
          }
      ]
  }

```
