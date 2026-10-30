# Rewriting a branch, a namespace or a file, and `edit.to`

``` ucm :hide
scratch/main> builtins.mergeio lib.builtins
```

``` unison :hide
app.a = 5 * 2
app.b = 3 * 2
other.c = 4 * 2
other.d = "no match"
```

``` ucm :hide
scratch/main> add
```

A rewrite in a file:

``` unison :hide
doubleRule a = @rewrite term a * 2 ==> a + a
```

`rewrite.find` lists the matches. Without a file, the rewrites are looked up in the latest typechecked file, or else in the branch.

``` ucm
scratch/main> load

  Loading changes detected in scratch.u.

  + doubleRule : Nat
                 -> Rewrites (Tuple (RewriteTerm Nat Nat) ())

  Run `update` to apply these changes to your codebase.

scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> rewrite.find :doubleRule

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. a
    2. b
    3. c
    4. doubleRule

  Tip: Try `edit 1` or `edit 1-4` to bring these into your
       scratch file.
```

Rewriting a namespace produces a file for review, and leaves the branch alone:

``` ucm
scratch/main> rewrite :doubleRule app

  ☝️

  I found and replaced matches in these definitions: app.a app.b

  The rewritten definitions are in
  `rewrites/0000-00-00-00-00-00000000.u`. Review them, typecheck
  that file with `load rewrites/0000-00-00-00-00-00000000.u` and
  run `update` when you are done.
```

``` unison :added-by-ucm rewrites/0000-00-00-00-00-00000000.u
-- Rewrote using: doubleRule
-- Review the definitions below, then typecheck this file (`load rewrites/0000-00-00-00-00-00000000.u`) and run `update` when you are done.
-- | Modified definition(s): app.a app.b

app.a : Nat
app.a =
  use Nat +
  5 + 5

app.b : Nat
app.b =
  use Nat +
  3 + 3
```

``` ucm
scratch/main> load

  Loading changes detected in
  rewrites/0000-00-00-00-00-00000000.u.

  ~ app.a : Nat
  ~ app.b : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.

scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view app.a other.c

  app.a : Nat
  app.a =
    use Nat +
    5 + 5

  other.c : Nat
  other.c =
    use Nat *
    4 * 2
```

Rewriting the current branch:

``` ucm
scratch/main> rewrite :doubleRule

  ☝️

  I found and replaced matches in these definitions: other.c

  The rewritten definitions are in
  `rewrites/0000-00-00-00-00-00000000.u`. Review them, typecheck
  that file with `load rewrites/0000-00-00-00-00-00000000.u` and
  run `update` when you are done.
```

``` unison :added-by-ucm rewrites/0000-00-00-00-00-00000000.u
-- Rewrote using: doubleRule
-- Review the definitions below, then typecheck this file (`load rewrites/0000-00-00-00-00-00000000.u`) and run `update` when you are done.
-- | Modified definition(s): other.c

other.c : Nat
other.c =
  use Nat +
  4 + 4
```

``` ucm
scratch/main> load

  Loading changes detected in
  rewrites/0000-00-00-00-00-00000000.u.

  ~ other.c : Nat

  ~ (modified)

  Run `update` to apply these changes to your codebase.

scratch/main> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> view other.c

  other.c : Nat
  other.c =
    use Nat +
    4 + 4
```

Once nothing matches, no file is produced:

``` ucm
scratch/main> rewrite :doubleRule

  😶️ I couldn't find any definitions that these rewrites match.
```

`edit.to` sends definitions to a file of your choice:

``` ucm
scratch/main> edit.to rewrites/mine.u app.a

  ☝️

  I added 1 definitions to the top of rewrites/mine.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.

scratch/main> edit.namespace.to rewrites/mine.u other

  ☝️

  I added 2 definitions to the top of rewrites/mine.u

  You can edit them there, then run `update` to replace the
  definitions currently in this namespace.
```

``` unison :added-by-ucm rewrites/mine.u
app.a : Nat
app.a =
  use Nat +
  5 + 5
```

``` unison :added-by-ucm rewrites/mine.u
other.c : Nat
other.c =
  use Nat +
  4 + 4

other.d : Text
other.d = "no match"
```

## Finding partially applied constructors and requests

A `case` rule that names a constructor or an ability request without its arguments, like `case Left ==> ()`, finds every
pattern that uses it (<https://github.com/unisonweb/unison/issues/5072>). It used to find nothing unless the arguments
were written out.

``` unison :hide
unique ability Emit where emit : Nat -> ()

eitherF : Either Nat Nat -> Nat
eitherF = cases
  Left a -> a
  Right b -> b

eitherG : Either Nat Nat -> Nat
eitherG = cases
  Left _ -> 0
  Right b -> b

optionH : Optional Nat -> Nat
optionH = cases
  Some n -> n
  None -> 0

handler : '{Emit} r -> [Nat]
handler thunk = handle !thunk with cases
  { r } -> []
  { emit n -> resume } -> n +: handler resume

plain : Nat -> Nat
plain n = n

findLeft = @rewrite case Left ==> ()
findLeftWithArgument a = @rewrite case Left a ==> ()
findSome = @rewrite case Some ==> ()
findEmit = @rewrite case Emit.emit ==> ()
```

``` ucm
scratch/main> load

  Loading changes detected in scratch.u.

  + ability Emit

  + eitherF              : Either Nat Nat -> Nat
  + eitherG              : Either Nat Nat -> Nat
  + findEmit             : Rewrites
                             (Tuple
                               (RewriteCase (Nat ->{Emit} ()) ())
                               ())
  + findLeft             : Rewrites
                             (Tuple
                               (RewriteCase (a -> Either a b) ())
                               ())
  + findLeftWithArgument : a
                           -> Rewrites
                             (Tuple
                               (RewriteCase (Either a b) ()) ())
  + findSome             : Rewrites
                             (Tuple
                               (RewriteCase (a -> Optional a) ())
                               ())
  + handler              : '{Emit} r -> [Nat]
  + optionH              : Optional Nat -> Nat
  + plain                : Nat -> Nat

  Run `update` to apply these changes to your codebase.

scratch/main> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

scratch/main> rewrite.find :findLeft

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. eitherF
    2. eitherG

  Tip: Try `edit 1` or `edit 1-2` to bring these into your
       scratch file.

scratch/main> rewrite.find :findLeftWithArgument

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. eitherF

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> rewrite.find :findSome

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. optionH

  Tip: Try `edit 1` to bring this into your scratch file.

scratch/main> rewrite.find :findEmit

  🔎

  These definitions from the current namespace (excluding `lib`) have matches:

    1. handler

  Tip: Try `edit 1` to bring this into your scratch file.
```
