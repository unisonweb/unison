# Unit tests for Any.unsafeExtract

``` ucm :hide
scratch/main> builtins.mergeio
scratch/main> load unison-src/transcripts-using-base/base.u
scratch/main> add
```

Any.unsafeExtract is a way to extract the value contained in an Any. This is unsafe because it allows the programmer to coerce a value into any type, which would cause undefined behaviour if used to coerce a value to the wrong type.

``` unison

test> Any.unsafeExtract.works =
  use Nat !=
  checks [1 == Any.unsafeExtract (Any 1),
          not (1 == Any.unsafeExtract (Any 2)),
          (Some 1) == Any.unsafeExtract (Any (Some 1))
         ]
```

``` ucm
scratch/main> add
```
