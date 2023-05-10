Unison should resolve types and terms by suffix even if it's a cyclic reference within the same file.

```unison
structural type deeply.nested.Tree a = 
  Leaf a
  | Node (Tree a) (Tree a)

foo.bar x = bar 1
```

Unison should *not* resolve names to definitions in the codebase if there's a matching name within the file.
Even if it's ambiguous, the definition in the file should take precedence.

```ucm
.> add
```

```unison
-- We alter the `Tree` type, all of the 'Tree' references should refer to the Tree in the file, not the one in the codebase.
structural type deeply.nested.Tree a = 
  Leaf a
  | Node (Tree a) a (Tree a)


-- We alter the foo.bar term. `bar` should refer to the 'foo.bar' within the file.
foo.bar x = bar 2
```

```ucm
.> update
-- Shouldn't see any hashes here
.> view deeply.nested.Tree foo.bar
```
