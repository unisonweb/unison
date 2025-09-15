``` ucm :hide
scratch/main> builtins.merge
```

```unison
type MyType = A | B

type AnotherType = C | D

myVal = A

myNum = 1

mySum = myNum + 2

myCase = cases
  A -> myNum
  B -> 2
```


```api
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=MyType
```

```api
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=myVal
```

```api
GET /api/projects/scratch/branches/main/getDefinitionDependents?name=myNum
```

