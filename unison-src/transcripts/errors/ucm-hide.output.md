### Transcript parser hidden errors

Dangerous scary words\!

When an error is encountered in a `ucm :hide` block
then the transcript parser should print the stanza
and surface a helpful message.

``` ucm :hide
> move.namespace foo bar
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
⚠️

The namespace foo doesn't exist.
```
