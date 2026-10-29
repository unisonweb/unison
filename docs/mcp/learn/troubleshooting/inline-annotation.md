A parse error that says "I was surprised to find a ':' here", or "expecting -> or newline or semicolon", often comes from a type written after an expression without parentheses.

The message points at the `:` of something like:

````
x = [] : [Nat]
````

To give an expression a type in the middle of a definition, put the whole expression and its type in parentheses: `x = ([] : [Nat])`. Or put a signature on the line above the definition, which is usually clearer (see [terms](learn:lang/terms)):

````
x : [Nat]
x = []
````

## Detail

When the annotated expression is followed by another definition, the error can instead point at that next definition, because the type runs on into it. The cause and the fix are the same.
