Regression test for https://github.com/unisonweb/unison/pull/2819

```ucm:hide
.> builtins.merge
```

```unison
hangExample : Boolean
hangExample =
  ("a long piece of text to hang the line" == "")
    && ("a long piece of text to hang the line" == "")
```

```ucm
.> add
.> view hangExample
```

