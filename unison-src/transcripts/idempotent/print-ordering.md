This transcript tests that let-rec bindings, ability lists and constructors are printed in order according to their
name, rather than their hashes.

``` ucm
scratch/main> builtins.mergeio

  Done.
```

``` unison :hide
-- Let-rec bindings should be ordered alphabetically by name when printed.
letRec = do
  z = do 1 Nat.+ y()
  x = do 2 Nat.+ z()
  y = do 3 Nat.+ x()
  x()

-- Non-cycle bindings dependent bindings are ordered in definition order
nonLetRec =
  z = 1
  x = 2 Nat.+ z
  y = 3 Nat.+ x
  y

-- This still isn't a let-rec, so it should keep its original order
nonDependent =
  z = 3
  x = 2
  y = 1
  x + y + z
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view letRec nonLetRec nonDependent

  letRec : 'Nat
  letRec = do
    use Nat +
    x = do 2 + z()
    y = do 3 + x()
    z = do 1 + y()
    x()

  nonDependent : Nat
  nonDependent =
    use Nat +
    z = 3
    x = 2
    y = 1
    x + y + z

  nonLetRec : Nat
  nonLetRec =
    use Nat +
    z = 1
    x = 2 + z
    y = 3 + x
    y
```

Type and ability constructors should be ordered alphabetically by name when printed.

``` unison :hide
type MyType =
    Y Nat
  | X Int
  | Z

ability MyAbility where
  def : Nat -> Nat
  ghi : Int -> Int
  abc : Nat -> Int
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view MyType MyAbility

  ability MyAbility where
    abc : Nat ->{MyAbility} Int
    def : Nat ->{MyAbility} Nat
    ghi : Int ->{MyAbility} Int

  type MyType = X Int | Y Nat | Z
```

Ability variables should be ordered alphabetically by name, followed by concrete ability heads in alphabetical order.

``` unison :hide

ability AnotherAbility where
  foo : Nat -> Nat

abilityTerm : '{MyAbility, z, x, AnotherAbility, y} Nat -> '{MyAbility, z, x, AnotherAbility, y} Nat
abilityTerm action = action
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view abilityTerm

  abilityTerm :
    '{x, y, z, AnotherAbility, MyAbility} Nat
    -> '{x, y, z, AnotherAbility, MyAbility} Nat
  abilityTerm action = action
```

-----

The following tests whether type constructors might be re-assigned to other constructors when updated.

``` unison :hide
-- Create a type with some identical constructors which are not in alphabetical order.
structural type MyType = C | B Nat |  A

toNat : MyType -> Nat
toNat = cases
  A -> 0
  B _ -> 1
  C -> 2

fromNat : Nat -> MyType
fromNat = cases
    0 -> A
    _ -> C
```

Add it to the codebase.

``` ucm :hide
scratch/main> update
```

Now when we edit it, the constructors would be printed in alphabetical order, BUT since 'A' and 'C' are identical
constructors, they should maintain their original ordering relative to one another. That is, 'C' should precede 'A'.
This behaviour ensures we don't accidentally swap meanings between identical constructors, e.g. swap True and False due
to alphabetical re-ordering.

``` ucm
scratch/main> edit MyType

  ☝️

  I added 1 definitions to the top of scratch.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm scratch.u
structural type MyType = B Nat | C | A
```

Even if we change the other constructor and update, the Nat mapping should be preserved.

``` unison :hide
structural type MyType = B Text | C | A
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  That's done. Now I'm making sure everything typechecks...

  Everything typechecks, so I'm saving the results...

  Done.
```

``` unison
> toNat A
> toNat C
> fromNat 0
> fromNat 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | > toNat A
          ⧩
          0

    2 | > toNat C
          ⧩
          2

    3 | > fromNat 0
          ⧩
          A

    4 | > fromNat 2
          ⧩
          C
```
