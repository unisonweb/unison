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
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=MyType
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "myVal",
                  "hash": "#88n7vpiqu9qhuj8v444iq3h7v93qvi7kei7dmmjojg3kc52v1aisg435t9bfedqakhk5fv8hu15daf379c7ovrfci9q627s6e3r7h1g",
                  "summary": {
                      "contents": [
                          {
                              "annotation": {
                                  "contents": "#0qbc2dfom7m4pputtdojo849g2mp5kkr00kvsvjktb07tcmo1jql53bg73bqiib35vja4a7059rcet0raf7jsh4d8vg5582ibinpqj8",
                                  "tag": "TypeReference"
                              },
                              "segment": "MyType"
                          }
                      ],
                      "tag": "UserObject"
                  },
                  "tag": "Plain"
              },
              "fqn": "myVal",
              "kind": "term",
              "projectRef": "scratch"
          },
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "myCase",
                  "hash": "#le6cur61p625qb7qfsaq836ln4l20u3ecngthot7io4p762ijb5t5hiv0c59eab9b8lktjp8l0j70r53ci43s89hjjik6hfsvkbjv28",
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
              "fqn": "myCase",
              "kind": "term",
              "projectRef": "scratch"
          }
      ]
  }

```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=myVal
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "myCase",
                  "hash": "#le6cur61p625qb7qfsaq836ln4l20u3ecngthot7io4p762ijb5t5hiv0c59eab9b8lktjp8l0j70r53ci43s89hjjik6hfsvkbjv28",
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
              "fqn": "myCase",
              "kind": "term",
              "projectRef": "scratch"
          }
      ]
  }

```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=myNum
RESPONSE:
  {
      "results": [
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "myCase",
                  "hash": "#le6cur61p625qb7qfsaq836ln4l20u3ecngthot7io4p762ijb5t5hiv0c59eab9b8lktjp8l0j70r53ci43s89hjjik6hfsvkbjv28",
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
              "fqn": "myCase",
              "kind": "term",
              "projectRef": "scratch"
          },
          {
              "branchRef": "main",
              "definition": {
                  "displayName": "mySum",
                  "hash": "#soiu5q8htcd38lhb0b434b8u1rlpq32v00cububck5so1ugs6avg5hjq99mvdm882c6mrcr8utq7v2hv7vm9sm6vk26jl6oscpt0okg",
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
              "fqn": "mySum",
              "kind": "term",
              "projectRef": "scratch"
          }
      ]
  }

```

``` api
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=mySum
RESPONSE:
  {
      "results": []
  }

```
