# Integration test: transcript

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

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      structural ability Break
      unique type MyBool
      main   : '{IO, Exception} ()
      resume : Request {g, Break} x -> x

```
```ucm
.> add

  ⍟ I've added these definitions:
  
    structural ability Break
    unique type MyBool
    main   : '{IO, Exception} ()
    resume : Request {g, Break} x -> x

.> compile main ./unison-cli/integration-tests/IntegrationTests/main

```
