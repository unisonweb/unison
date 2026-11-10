Starting codebase: `scratch/main` with `@unison/base`, plus one existing function and its test, so that `main` has some history and a test of its own. Offline. The worker starts on `main`.

``` unison
Text.whisper : Text -> Text
Text.whisper t = Text.toLowercase t

test> Text.whisper.tests.basic = test.verify do
  ensureEqual (Text.whisper "HeLLo") "hello"
```

``` ucm
scratch/main> add
```
