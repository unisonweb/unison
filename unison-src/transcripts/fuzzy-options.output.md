# Test that the options selector for fuzzy finding is working as expected for different argument types.

If an argument is required but doesn't have a fuzzy resolver, the command should just print the help.


```ucm
-- The second argument of move.term is a 'new-name' and doesn't have a fuzzy resolver
.> move.term

```

```



🛑

The transcript was expecting an error in the stanza above, but did not encounter one.
