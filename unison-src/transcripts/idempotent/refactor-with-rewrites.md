# A type-changing refactoring with rewrites

The recipe in `docs/mcp/learn/skills/refactor.md`: a function gets an extra argument and a type's constructor swaps its
fields. The new versions get a `_v2` suffix and go in a work file with the uses to change and the rewrite rules; `rewrite`
rewrites the file; the suffix is stripped from the result and it is updated.

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison :hide
foo : Nat -> Nat -> Nat
foo x y = x + y

useFull = foo 1 2
usePartial = List.map (foo 1) [1,2]
useBare : Nat -> Nat -> Nat
useBare = foo

type Blah = C1 Nat Text | C2

mk : Nat -> Text -> Blah
mk n t = C1 n t

mk2 : Nat -> Text -> Blah
mk2 = C1

un : Blah -> Nat
un = cases
  C1 n _ -> n
  C2 -> 0
```

``` ucm :hide
scratch/main> add
```

The work file holds the new versions (the constructors are suffixed too, so that patterns on the old ones stay
unambiguous), the uses that `dependents foo` and `dependents Blah` listed (`edit.to work.u 1-` adds them to the file),
and the rules, in a namespace of their own:

``` unison :hide work.u
useBare : Nat -> Nat -> Nat
useBare = foo

useFull : Nat
useFull = foo 1 2

usePartial : [Nat]
usePartial = List.map (foo 1) [1, 2]

mk : Nat -> Text -> Blah
mk n t = C1 n t

mk2 : Nat -> Text -> Blah
mk2 = C1

un : Blah -> Nat
un = cases
  C1 n _ -> n
  C2 -> 0

foo_v2 : Nat -> Nat -> Nat -> Nat
foo_v2 x y z = x + y + z

type Blah_v2 = C1_v2 Text Nat | C2_v2

rules.r1 x y = @rewrite term foo x y ==> foo_v2 x y 0
rules.r2 x = @rewrite term foo x ==> (y -> foo_v2 x y 0)
rules.r3 = @rewrite term foo ==> (x y -> foo_v2 x y 0)

rules.b1 n t = @rewrite
  signature Blah ==> Blah_v2
  case Blah.C1 n t ==> C1_v2 t n
  case Blah.C2 ==> C2_v2
  term Blah.C1 n t ==> C1_v2 t n
  term Blah.C1 ==> (n t -> C1_v2 t n)
  term Blah.C2 ==> C2_v2
```

The rules refer to `foo_v2` and `Blah_v2`, which only the file defines, and the rewritten code refers to them by name. The
file is rewritten in place, and the version before is saved under `rewrites/`.

``` ucm
scratch/main> rewrite work.u:rules.r1,rules.r2,rules.r3,rules.b1 work.u

  ☝️

  I found and replaced matches in these definitions: mk mk2 un
  useBare useFull usePartial

  I rewrote `work.u` in place; the version before is saved as
  `rewrites/0000-00-00-00-00-work-00000000.u`. Review the file,
  typecheck it with `load work.u` and run `update` when you are
  done.

scratch/main> load work.u

  Loading changes detected in work.u.

  + type Blah_v2

  + foo_v2   : Nat -> Nat -> Nat -> Nat
  + rules.b1 : Nat
               -> Text
               -> Rewrites
                 ( RewriteSignature Blah Blah_v2,
                   RewriteCase Blah Blah_v2,
                   RewriteCase Blah Blah_v2,
                   RewriteTerm Blah Blah_v2,
                   RewriteTerm
                   (Nat -> Text -> Blah)
                   (Nat -> Text -> Blah_v2),
                   RewriteTerm Blah Blah_v2)
  + rules.r1 : Nat
               -> Nat
               -> Rewrites (Tuple (RewriteTerm Nat Nat) ())
  + rules.r2 : Nat
               -> Rewrites
                 (Tuple
                   (RewriteTerm (Nat -> Nat) (Nat -> Nat)) ())
  + rules.r3 : Rewrites
                 (Tuple
                   (RewriteTerm
                     (Nat -> Nat -> Nat) (Nat -> Nat -> Nat))
                   ())
  ~ mk         : Nat -> Text -> Blah_v2
  ~ mk2        : Nat -> Text -> Blah_v2
  ~ un         : Blah_v2 -> Nat
  ~ useBare    : Nat -> Nat -> Nat
  ~ useFull    : Nat
  ~ usePartial : [Nat]

  + (added), ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` unison :added-by-ucm rewrites/0000-00-00-00-00-work-00000000.u
useBare : Nat -> Nat -> Nat
useBare = foo

useFull : Nat
useFull = foo 1 2

usePartial : [Nat]
usePartial = List.map (foo 1) [1, 2]

mk : Nat -> Text -> Blah
mk n t = C1 n t

mk2 : Nat -> Text -> Blah
mk2 = C1

un : Blah -> Nat
un = cases
  C1 n _ -> n
  C2 -> 0

foo_v2 : Nat -> Nat -> Nat -> Nat
foo_v2 x y z = x + y + z

type Blah_v2 = C1_v2 Text Nat | C2_v2

rules.r1 x y = @rewrite term foo x y ==> foo_v2 x y 0
rules.r2 x = @rewrite term foo x ==> (y -> foo_v2 x y 0)
rules.r3 = @rewrite term foo ==> (x y -> foo_v2 x y 0)

rules.b1 n t = @rewrite
  signature Blah ==> Blah_v2
  case Blah.C1 n t ==> C1_v2 t n
  case Blah.C2 ==> C2_v2
  term Blah.C1 n t ==> C1_v2 t n
  term Blah.C1 ==> (n t -> C1_v2 t n)
  term Blah.C2 ==> C2_v2
```

``` unison :added-by-ucm work.u
useBare : Nat -> Nat -> Nat
useBare x y = foo_v2 x y 0

useFull : Nat
useFull = foo_v2 1 2 0

usePartial : [Nat]
usePartial = List.map (y -> foo_v2 1 y 0) [1, 2]

mk : Nat -> Text -> Blah_v2
mk n t = Blah_v2.C1_v2 t n

mk2 : Nat -> Text -> Blah_v2
mk2 n t = Blah_v2.C1_v2 t n

un : Blah_v2 -> Nat
un = cases
  Blah_v2.C1_v2 _ n -> n
  Blah_v2.C2_v2     -> 0

foo_v2 : Nat -> Nat -> Nat -> Nat
foo_v2 x y z = x + y + z

type Blah_v2 = C1_v2 Text Nat | C2_v2

rules.r1 x y = @rewrite term foo x y ==> foo_v2 x y 0

rules.r2 x = @rewrite term foo x ==> y -> foo_v2 x y 0

rules.r3 = @rewrite term foo ==> x y -> foo_v2 x y 0

rules.b1 n t =
  use Blah_v2 C1_v2 C2_v2
  @rewrite
    signature Blah ==> Blah_v2
    case C1 n t ==> C1_v2 t n
    case C2 ==> C2_v2
    term C1 n t ==> C1_v2 t n
    term C1 ==> n t -> C1_v2 t n
    term C2 ==> C2_v2
```

Stripping `_v2` from the file and deleting the rules gives the final versions, and `update` applies cleanly, since every
use is in the file:

``` unison
useBare : Nat -> Nat -> Nat
useBare x y = foo x y 0

useFull : Nat
useFull = foo 1 2 0

usePartial : [Nat]
usePartial = List.map (y -> foo 1 y 0) [1, 2]

mk : Nat -> Text -> Blah
mk n t = Blah.C1 t n

mk2 : Nat -> Text -> Blah
mk2 n t = Blah.C1 t n

un : Blah -> Nat
un = cases
  Blah.C1 _ n -> n
  Blah.C2     -> 0

foo : Nat -> Nat -> Nat -> Nat
foo x y z = x + y + z

type Blah
  = C1 Text Nat
  | C2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  ~ type Blah

  ~ foo        : Nat -> Nat -> Nat -> Nat
  ~ mk         : Nat -> Text -> Blah
  ~ mk2        : Nat -> Text -> Blah
  ~ un         : Blah -> Nat
  ~ useBare    : Nat -> Nat -> Nat
  ~ useFull    : Nat
  ~ usePartial : [Nat]

  ~ (modified)

  Run `update` to apply these changes to your codebase.
```

``` ucm
scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view foo un

  foo : Nat -> Nat -> Nat -> Nat
  foo x y z =
    use Nat +
    x + y + z

  un : Blah -> Nat
  un = cases
    C1 _ n -> n
    C2     -> 0
```
