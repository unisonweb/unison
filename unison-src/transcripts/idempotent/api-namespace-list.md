# Namespace list api

``` ucm :hide
> builtins.mergeio
```

``` unison
{{ Documentation }}
nested.names.x = 42

nested.names.readme = {{ I'm a readme! }}
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + nested.names.readme : Doc2
  + nested.names.x      : Nat
  + nested.names.x.doc  : Doc2

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

``` api
GET /api/projects/scratch/branches/main/list?namespace=nested.names
RESPONSE:
  {
      "namespaceListingChildren": [
          {
              "contents": {
                  "termHash": "#ddmmatmmiqsts2ku0i02kntd0s7rvcui4nn1cusio8thp9oqhbtilvcnhen52ibv43kr5q83f5er5q9h56s807k17tnelnrac7cch8o",
                  "termName": "readme",
                  "termTag": "Doc",
                  "termType": [
                      {
                          "annotation": {
                              "contents": "#ej86si0ur1",
                              "tag": "HashQualifier"
                          },
                          "segment": "#ej86si0ur1"
                      }
                  ]
              },
              "tag": "TermObject"
          },
          {
              "contents": {
                  "termHash": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
                  "termName": "x",
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
              },
              "tag": "TermObject"
          },
          {
              "contents": {
                  "namespaceHash": "#n1egracfeljprftoktbjcase2hs4f4p8idbhs5ujipl42agld1810hrq9t7p7ped16aagni2cm1fjcjhho770jh80ipthhmg0cnsur0",
                  "namespaceName": "x",
                  "namespaceSize": 1
              },
              "tag": "Subnamespace"
          }
      ],
      "namespaceListingFQN": "nested.names",
      "namespaceListingHash": "#oms19b4f9s3c8tb5skeb8jii95ij35n3hdg038pu6rv5b0fikqe4gd7lnu6a1i6aq5tdh2opdo4s0sfrupvk6vfkr9lf0n752gbl8o0"
  }

GET /api/projects/scratch/branches/main/list?namespace=names&relativeTo=nested
RESPONSE:
  {
      "namespaceListingChildren": [
          {
              "contents": {
                  "termHash": "#ddmmatmmiqsts2ku0i02kntd0s7rvcui4nn1cusio8thp9oqhbtilvcnhen52ibv43kr5q83f5er5q9h56s807k17tnelnrac7cch8o",
                  "termName": "readme",
                  "termTag": "Doc",
                  "termType": [
                      {
                          "annotation": {
                              "contents": "#ej86si0ur1",
                              "tag": "HashQualifier"
                          },
                          "segment": "#ej86si0ur1"
                      }
                  ]
              },
              "tag": "TermObject"
          },
          {
              "contents": {
                  "termHash": "#qkhkl0n238s1eqibd1ecb8605sqj1m4hpoaag177cu572otqlaf1u28c8suuuqgljdtthsjtr07rv04np05o6oa27ml9105k7uas0t8",
                  "termName": "x",
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
              },
              "tag": "TermObject"
          },
          {
              "contents": {
                  "namespaceHash": "#n1egracfeljprftoktbjcase2hs4f4p8idbhs5ujipl42agld1810hrq9t7p7ped16aagni2cm1fjcjhho770jh80ipthhmg0cnsur0",
                  "namespaceName": "x",
                  "namespaceSize": 1
              },
              "tag": "Subnamespace"
          }
      ],
      "namespaceListingFQN": "nested.names",
      "namespaceListingHash": "#oms19b4f9s3c8tb5skeb8jii95ij35n3hdg038pu6rv5b0fikqe4gd7lnu6a1i6aq5tdh2opdo4s0sfrupvk6vfkr9lf0n752gbl8o0"
  }

```
