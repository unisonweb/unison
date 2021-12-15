# Globbing 

## Overview

This allows quickly selecting terms, types, and namespaces for any "bulk" commands.

* Currently supports up to one wildcard PER SEGMENT; Each segment can have its own wildcard if you really want and it'll still be performant. E.g. `.base.?.to?`
* Can have a prefix, suffix or infix wildcard! E.g. `to?` or `?List` or `to?With!`
* I went with `?` instead of `*` for the wildcard symbol since `?` isn't currently a valid symbol name. This may cause some confusion since it differs from bash globbing though; so if anyone has thoughts/concerns about how to better handle this I'd love to hear them.
* Commands can select which targets they want globs to expand to; e.g. `cd` should only glob for namespace, `view` should only glob to terms & types.

## Demo

Add some definitions which we can match over:
```unison:hide
convertToThing = 1
convertFromThing = 2
otherTerm = 3

-- Nested definitions
nested.toList = 4
nested.toMap = 5
othernest.toList = 6
othernest.toMap = 7
```

```ucm:hide
.> add
```

Globbing as a prefix, infix, or suffix wildcard.

```ucm
.> view convert?
.> view convert?Thing
.> view ?Thing
```

Globbing can occur in any name segment.

```ucm
.> view ?.toList
.> view nested.to?
```

You may have up to one glob per name segment.

```ucm
.> view ?.to?
```


Globbing only expands to the appropriate argument type.

E.g. `view` should not see glob expansions for namespaces.
This should expand to only the otherTerm.

```ucm
.> view other?
```

Globbing should work from within a namespace with both absolute and relative patterns.

```ucm
.nested> view .othernest.to?
.nested> view to?
```
