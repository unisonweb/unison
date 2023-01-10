```unison:hide
structural type Optional a = Some a | None
```

```ucm:hide
.> add
```

## Common syntax errors should fail to parse

```unison:error
x = match Some a with
      None -> 
        1
      Some _
        2
```

```unison:error
x = match Some a with
      None -> 1
           -> 2
           -> 3
```

Can't have guards following an unguarded expression

```unison:error
x = match Some a with
      None     -> 1
        | true -> 2
```

Must have at least one pattern case.

```unison:error
x = match Some a with
```
