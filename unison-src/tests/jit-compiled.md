```ucm :hide
> builtins.merge lib.builtin
```

A compiled program (`run.compiled`) goes through the JIT like a codebase run
does. The loop is hot enough to be compiled in every mode, and it matches on a
data type declared here, so that the compiled file carries the constructor
arities the JIT needs (unison-runtime/src/Unison/Runtime/JIT/design.md). CI
runs the `.uc` with `UNISON_JIT=eager UNISON_JIT_LOG=1` and checks the log and
the printed sum.

```unison :hide
type Shape = Circle Nat | Rect Nat Nat

area : Shape -> Nat
area = cases
  Circle r -> 3 * r * r
  Rect w h -> w * h

shape : Nat -> Shape
shape n = if Nat.mod n 2 == 0 then Circle n else Rect n (n + 1)

sumAreas : Nat -> Nat -> Nat
sumAreas n acc =
  if n == 0 then acc
  else sumAreas (Nat.drop n 1) (acc + area (shape n))

printLine : Text ->{IO, Exception} ()
printLine t = match putBytes.impl (stdHandle StdOut) (Text.toUtf8 (t ++ "\n")) with
  Right () -> ()
  Left f -> Exception.raise f

main : '{IO, Exception} ()
main = do printLine (Nat.toText (sumAreas 1000000 0))
```

```ucm :hide
> add
> compile main jit-compiled
```
