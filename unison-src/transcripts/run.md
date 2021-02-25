```ucm:hide
.> builtins.merge
.> builtins.mergeio
```

In a scratch file, we'll define two terms.
- `runnable`: a properly typed term that can be `run`
- `badtype`: an improperly typed term that cannot be `run`

```unison scratch.u
runnable = '(printLine "hello!")
badtype = 'printLine "hello!"
```

We'll see when we `run` the `runnable` term, we get the successful "hello!" we expect:

**There is a bug here! https://github.com/unisonweb/unison/issues/1800**
(this should not error)

```ucm:error
.> run runnable
```

When we run the term with the bad type, we get an error message that lets us know that Unison found the term, but the type is not `run`able:

```ucm:error
.> run badtype
```

When we run a term that Unison cannot find at all, we get an error message that the term could not be found:

```ucm:error
.> run notfound
```

Now let's add these terms to the codebase and clear our scratchfile, to show that the behavior is consistent if the terms are found inside the codebase.

```ucm
.> add
```

```unison scratch.u
```

**(The bug again!)**

```ucm:error
.> run runnable
```

```ucm:error
.> run badtype
```
