Regression test for https://github.com/unisonweb/unison/pull/2819

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

