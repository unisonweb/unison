A GADT is a type whose constructors each say what their own result type is, so the type can carry an index, such as an expression that is known to produce a `Nat`.

- Declare it with `type Name params where`, then give each constructor its full type, including its result type:

  ````
  ``` unison
  type Expr a where
    NatLit : Nat -> Expr Nat
    BoolLit : Boolean -> Expr Boolean
    Add : Expr Nat -> Expr Nat -> Expr Nat
    If : Expr Boolean -> Expr a -> Expr a -> Expr a
  ```
  ````

  The parameter `a` is the index. Each constructor fixes it differently, and `If` works for any index but needs both branches to share it. `Add (NatLit 1) (BoolLit true)` doesn't typecheck.
- [Matching](learn:lang/cases) a constructor tells the compiler what the index is in that case, so a function can return the type the value describes:

  ````
  ``` unison
  eval : Expr a -> a
  eval = cases
    NatLit n -> n
    BoolLit b -> b
    Add x y -> eval x + eval y
    If c t f -> if eval c then eval t else eval f
  ```
  ````

- Write a type signature on a function whose match depends on the index, like `eval`. Without it the types aren't known when the cases are checked, and it fails. A match whose cases all give the same type needs none.
- Like other types, a GADT is unique by default (see [data types](learn:lang/data)). `structural` can precede it.

## Detail

**A compound index.** The index can be a structure, which matching takes apart:

````
``` unison
type Two a where
  Item : Nat -> Two Nat
  Pair : Two x -> Two y -> Two (x, y)

evalTwo : Two a -> a
evalTwo = cases
  Item n -> n
  Pair l r -> (evalTwo l, evalTwo r)

> evalTwo (Pair (Item 1) (Item 2))
```
````

This gives `(1, 2)`.

**Impossible cases can be left out.** When the index is known, constructors that can't build it don't need a case. A function on `Expr Nat` needs no `BoolLit` case:

````
``` unison
isNatLiteral : Expr Nat -> Boolean
isNatLiteral = cases
  NatLit _ -> true
  Add _ _ -> false
  If _ _ _ -> false
```
````

The same holds for several arguments with a shared index. With `type Tagged a where IsNat : Tagged Nat` and `IsBool : Tagged Boolean`, `agree : Tagged a -> Tagged a -> Boolean` needs only `IsNat, IsNat` and `IsBool, IsBool`.

**Equality witnesses.** A constructor can require two indices to be the same type. Matching it makes them the same in that case:

````
``` unison
type Equ a b where
  Refl : Equ a a

coerce : Equ a b -> a -> b
coerce = cases
  Refl -> (x -> x)
```
````

**Names.** A constructor whose name is also a constructor in a library (such as `One` or `Single`) can make a pattern ambiguous. Choose names that are specific to the type.
