``` ucm
> builtins.mergeio lib.builtins

  Done.
```

``` unison
{{A doc directly in the namespace.}}
some.ns.direct = 1

{{A doc pretty deeply nested in the namespace.}}
some.ns.pretty.deeply.nested = 2

{{A doc outside the namespace.}}
some.outside = 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + some.ns.direct                   : Nat
  + some.ns.direct.doc               : Doc2
  + some.ns.pretty.deeply.nested     : Nat
  + some.ns.pretty.deeply.nested.doc : Doc2
  + some.outside                     : Nat
  + some.outside.doc                 : Doc2

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Done.

> docs.to-html some.ns unison-src/transcripts-manual/docs.to-html
```
