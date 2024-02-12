
Fetch base, then fetch the compiler, then build the generated
libraries in the racket directory.

```ucm
.> pull @unison/base/releases/2.5.0 .base

  Downloaded 12426 entities.

  ✅

  Successfully pulled into .base, which was empty.

.> compile.native.fetch

  Downloaded 1465 entities.

  ✅

  Successfully updated .unison.internal from
  @unison/internal/releases/0.0.10.

.> compile.native.genlibs scheme-libs/racket

```
After executing this, `scheme-libs/racket` will contain the full
complement of unison libraries for a given combination of ucm version
and @unison/internal version.

To set up racket to use these files, we need to create a package with
them. This is accomplished by running.

    raco pkg install -t dir unison

in the directory where the `unison directory is located. Then the
runtime executable can be built with

    raco exe scheme-libs/racket/ucr.rkt

and a distributable directory can be produced with:

    raco distribute <output-dir> scheme-libs/racket/ucr

At that point, <output-dir> should contain the executable and all
dependencies necessary to run it.
