```ucm
diffs/main> builtins.merge
```

```unison
term =
  _ = "Here's some text"
  1 + 1

type Type = Type Nat
```

```ucm
diffs/main> add
diffs/main> branch.create new
```

```unison
term =
  _ = "Here's some different text"
  1 + 2

type Type a = Type a Text
```

```ucm
diffs/new> update
```

Diff terms

```api
GET /api/projects/diffs/diff/terms?oldBranchRef=main&newBranchRef=new&oldTerm=term&newTerm=term
```

Diff types

```api
GET /api/projects/diffs/diff/types?oldBranchRef=main&newBranchRef=new&oldType=Type&newType=Type
```
