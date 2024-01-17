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

unique type MyBool = MyTrue | MyFalse

structural ability Break where
  break : ()

resume = cases
  { x } -> id x
  { break  -> k } ->
    void 5
    handle k () with resume

main : '{IO, Exception} ()
main = do
  match MyTrue with
    MyTrue -> match 0 with
      0 ->
        handle
          break
          printLine "Hello, world!"
        with resume
      _ -> ()
    _ -> ()
```

```ucm
.> add
.> compile main ./unison-cli/integration-tests/IntegrationTests/main
```
