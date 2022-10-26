# Integration test: transcript

```ucm:hide
.> builtins.mergeio
.> load ./unison-src/transcripts-using-base/base.u
```

```ucm:hide
.> builtins.mergeio
.> load ./unison-src/transcripts-using-base/base.u
.> add
```

```unison
use .builtin

main : '{IO, Exception} ()
main = '(printLine "Hello, world!")
```

```ucm
.> add
.> compile main ./unison-cli/integration-tests/IntegrationTests/main
```
