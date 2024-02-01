First we'll set up two libraries, and then we'll use them in some projects and show what `names` are deep-loaded for them.

Our two "libraries":
```unison:hide
text.a = 1
text.b = 2
text.c = 3

http.x = 6
http.y = 7
http.z = 8
```

```ucm:hide
.> add
```

Our `app1` project includes the text library twice and the http library twice as direct dependencies.
```ucm
.app1> fork .text lib.text_v1
.app1> fork .text lib.text_v2
.app1> fork .http lib.http_v3
.app1> fork .http lib.http_v4
```

As such, we see two copies of `a` and two copies of `x` via these direct dependencies.
```ucm
.app1> names a
.app1> names x
```

Our `app2` project includes the `http` library twice as direct dependencies, and once as an indirect dependency via `webutil`.
It also includes the `text` library twice as indirect dependencies via `webutil`
```ucm
.app2> fork .http lib.http_v1
.app2> fork .http lib.http_v2
.app2> fork .text lib.webutil.lib.text_v1
.app2> fork .text lib.webutil.lib.text_v2
.app2> fork .http lib.webutil.lib.http
```

Now we see two copies of `x` via direct dependencies on `http`, and one copy of `a` via indirect dependency on `text` via `webutil`.
We see neither the second indirect copy of `a` nor the indirect copy of `x` via webutil because we already have names for them.
```ucm
.app2> names a
.app2> names x
```
