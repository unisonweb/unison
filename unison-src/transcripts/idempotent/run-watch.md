# `run>` watch expressions

A `run>` watch expects an expression of type `'{IO, Exception} a`, forces it
(running its side effects), and—unlike a regular `>` watch—is never cached, so
it re-evaluates on every save. (See `transcripts-using-base/run-watch.md` for a
demonstration of the side-effecting, re-evaluate-every-save behavior.)

``` ucm :hide
scratch/main> builtins.merge
```

## It forces the thunk and shows the result

A pure thunk is accepted (it fits `'{IO, Exception} a`), forced, and its result
shown:

``` unison
run> do 1 + 1
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  No changes found.

    1 | run> do 1 + 1
             ⧩
             2
```

## It coexists with regular watches and resolves file references

A `run>` watch can appear alongside ordinary definitions and regular `>`
watches in the same file, and resolves references to file definitions:

``` unison
x = 100

> x + 1
run> do x + 2
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + x : Nat

  Run `update` to apply these changes to your codebase.

    3 | > x + 1
          ⧩
          101

    4 | run> do x + 2
             ⧩
             102
```

## It rejects expressions that aren't a delayed `'{IO, Exception} a`

``` unison :error
run> 3 + 4
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  I was expecting this run> watch expression to be a subtype of:

    '{IO, Exception} a

  but it actually has type:

    Nat

      1 | run> 3 + 4
```
