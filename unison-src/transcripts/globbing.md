# Globbing 

## Overview

This allows quickly selecting terms, types, and namespaces for any "bulk" commands.

* Currently supports up to one wildcard PER SEGMENT; Each segment can have its own wildcard if you really want and it'll still be performant. E.g. `.base.?.to?`
* Can have a prefix, suffix or infix wildcard! E.g. `to?` or `?List` or `to?With!`
* I went with `?` instead of `*` for the wildcard symbol since `?` isn't currently a valid symbol name. This may cause some confusion since it differs from bash globbing though; so if anyone has thoughts/concerns about how to better handle this I'd love to hear them.
* Commands can select which targets they want globs to expand to; e.g. `cd` should only glob for namespace, `view` should only glob to terms & types.

## Demo

```unison
tweedleDee = 1
tweedleDum = 2
```

```ucm
.> add
.> view tweedle?
```
