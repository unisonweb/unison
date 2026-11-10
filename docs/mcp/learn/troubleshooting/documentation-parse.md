A parse error inside a documentation block, "I was surprised to find an end of input here" with a list that includes "block eval (syntax: a fenced code block)", means the documentation's own code fence ended your `unison` block early.

Documentation can contain fenced code blocks (see [documentation](learn:lang/documentation)). If the `unison` block of your transcript is fenced with three backticks, the first fence inside the documentation closes it, and the rest of the documentation is cut off.

Open and close the `unison` block with four backticks:

`````
```` unison
double.doc = {{

``double n`` doubles `n`. For example:

```
double 2
```

}}
````
`````

A misspelled directive, such as `@typechecked` for `@typecheck`, gives a different parse error, "expecting ``` or white space". Check the spelling of `@typecheck`, `@source` and `@foldedSource`.
