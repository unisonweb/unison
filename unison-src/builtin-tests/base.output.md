When this file is modified, CI will create a new codebase and re-run this;
otherwise it may reuse a previously cached codebase.

Thus, make sure the contents of this file define the contents of the cache
(e.g. don't pull `latest`.)

```ucm
.> pull unison.public.base.releases.v2_0_0 .base

  Downloaded 11980 entities.

  ✅
  
  Successfully pulled into .base, which was empty.

.> compile.native.fetch

  Downloaded 65491 entities.

  ✅
  
  Successfully updated .unison.internal from
  unison.public.internal.trunk.

```
