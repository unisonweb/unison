# The `todo` and `bug` builtin

```ucm:hide
.> builtins.merge
```

`todo` and `bug` have type `a -> b`. They take a message or a value of type `a` and crash during runtime displaying `a` in ucm.
```unison:error
> todo "implement me later"
```
```unison:error
> bug "there's a bug in my code"
```

## Todo
`todo` is useful if you want to come back to a piece of code later but you want your project to compile.
```unison
complicatedMathStuff x = todo "Come back and to something with x here"
```

## Bug
`bug` is used to indicate that a particular branch is not expected to execute.
```unison
test = match true with
    true -> "Yay"
    false -> bug "Wow, that's unexpected"
```
