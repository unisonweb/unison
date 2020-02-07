##### Find by type support

`find : a -> b` should find the `bug` and `todo` functions.

```ucm
.> find : a -> b

  1. builtin.bug : a -> b
  2. builtin.todo : a -> b
  

```
`find : b -> a` should also find the `bug` and `todo` functions.

```ucm
.> find : b -> a

  1. builtin.bug : a -> b
  2. builtin.todo : a -> b
  

```
`foldl` should be found if `a` and `b` are swapped.

```ucm
.> find : (b -> a -> b) -> b -> [a] -> b

  1. foldl : (b ->{𝕖} a ->{𝕖} b) -> b -> [a] ->{𝕖} b
  

```
```ucm
.> find : (a -> b -> a) -> a -> [b] -> a

  1. foldl : (b ->{𝕖} a ->{𝕖} b) -> b -> [a] ->{𝕖} b
  

```
The `flip` function should be found regardless of the naming of the types.

```ucm
.> find : (a -> b -> c) -> (b -> a -> c)

  1. flip : (a ->{𝕖} b ->{𝕖} c) -> b -> a ->{𝕖} c
  

.> find : (b -> a -> c) -> (a -> b -> c)

  1. flip : (a ->{𝕖} b ->{𝕖} c) -> b -> a ->{𝕖} c
  

.> find : (c -> b -> a) -> (b -> c -> a)

  1. flip : (a ->{𝕖} b ->{𝕖} c) -> b -> a ->{𝕖} c
  

.> find : (d -> e -> f) -> (e -> d -> f)

  1. flip : (a ->{𝕖} b ->{𝕖} c) -> b -> a ->{𝕖} c
  

```
