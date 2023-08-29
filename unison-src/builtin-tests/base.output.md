When this file is modified, CI will create a new codebase and re-run this;
otherwise it may reuse a previously cached codebase.

Thus, make sure the contents of this file define the contents of the cache
(e.g. don't pull `latest`.)

```ucm
.> pull @unison/base/releases/2.5.0 .base

  Downloaded 12426 entities.

  ✅
  
  Successfully pulled into .base, which was empty.

.> compile.native.fetch

  Downloaded 1549 entities.

  ✅
  
  Successfully updated .unison.internal from
  @unison/internal/releases/0.0.3.

```
