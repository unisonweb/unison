First we'll set up two libraries, and then we'll use them in some projects and show what `names` are deep-loaded for them.

Our two "libraries":
```unison
text.a = 1
text.b = 2
text.c = 3

http.x = 6
http.y = 7
http.z = 8
```

Our `app1` project includes the text library twice and the http library twice as direct dependencies.
```ucm
  ☝️  The namespace .app1 is empty.

.app1> fork .text lib.text_v1

  Done.

.app1> fork .text lib.text_v2

  Done.

.app1> fork .http lib.http_v3

  Done.

.app1> fork .http lib.http_v4

  Done.

```
As such, we see two copies of `a` and two copies of `x` via these direct dependencies.
```ucm
.app1> names a

  Term
  Hash:   #gjmq673r1v
  Names:  lib.text_v1.a lib.text_v2.a
  
  Tip: Use `names.global` to see more results.

.app1> names x

  Term
  Hash:   #nsmc4p1ra4
  Names:  lib.http_v3.x lib.http_v4.x
  
  Tip: Use `names.global` to see more results.

```
Our `app2` project includes the `http` library twice as direct dependencies, and once as an indirect dependency via `webutil`.
It also includes the `text` library twice as indirect dependencies via `webutil`
```ucm
  ☝️  The namespace .app2 is empty.

.app2> fork .http lib.http_v1

  Done.

.app2> fork .http lib.http_v2

  Done.

.app2> fork .text lib.webutil.lib.text_v1

  Done.

.app2> fork .text lib.webutil.lib.text_v2

  Done.

.app2> fork .http lib.webutil.lib.http

  Done.

```
Now we see two copies of `x` via direct dependencies on `http`, and one copy of `a` via indirect dependency on `text` via `webutil`.
We see neither the second indirect copy of `a` nor the indirect copy of `x` via webutil because we already have names for them.
```ucm
.app2> names a

  Term
  Hash:   #gjmq673r1v
  Names:  lib.webutil.lib.text_v1.a
  
  Tip: Use `names.global` to see more results.

.app2> names x

  Term
  Hash:   #nsmc4p1ra4
  Names:  lib.http_v1.x lib.http_v2.x
  
  Tip: Use `names.global` to see more results.

```
