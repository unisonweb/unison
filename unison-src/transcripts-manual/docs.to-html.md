``` ucm
> builtins.mergeio lib.builtins
```

``` unison
{{A doc directly in the namespace.}}
some.ns.direct = 1

{{A doc pretty deeply nested in the namespace.}}
some.ns.pretty.deeply.nested = 2

{{A doc outside the namespace.}}
some.outside = 3
```

``` ucm
> add
> docs.to-html some.ns unison-src/transcripts-manual/docs.to-html
```
