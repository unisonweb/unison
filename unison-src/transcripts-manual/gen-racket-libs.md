
Fetch base, then fetch the compiler, then build the generated
libraries in the racket directory.

```ucm
.> pull @unison/base/releases/2.5.0 .base
.> compile.native.fetch
.> compile.native.genlibs scheme-libs/racket
```
