First we'll set up two libraries, and then we'll use them in some projects and show what `names` are deep-loaded for them.

Our two "libraries":
``` unison :hide
text.a = 1
text.b = 2
text.c = 3

http.x = 6
http.y = 7
http.z = 8
```

``` ucm :hide
scratch/main> add
scratch/main> branch /app1
scratch/main> branch /app2
```

Our `app1` project includes the text library twice and the http library twice as direct dependencies.
``` ucm
scratch/app1> fork text lib.text_v1
scratch/app1> fork text lib.text_v2
scratch/app1> delete.namespace text
scratch/app1> fork http lib.http_v3
scratch/app1> fork http lib.http_v4
scratch/app1> delete.namespace http
```

As such, we see two copies of `a` and two copies of `x` via these direct dependencies.
``` ucm
scratch/app1> names a
scratch/app1> names x
```

Our `app2` project includes the `http` library twice as direct dependencies, and once as an indirect dependency via `webutil`.
It also includes the `text` library twice as indirect dependencies via `webutil`
``` ucm
scratch/app2> fork http lib.http_v1
scratch/app2> fork http lib.http_v2
scratch/app2> fork text lib.webutil.lib.text_v1
scratch/app2> fork text lib.webutil.lib.text_v2
scratch/app2> fork http lib.webutil.lib.http
scratch/app2> delete.namespace http
scratch/app2> delete.namespace text
```

Now we see two copies of `x` via direct dependencies on `http`, and one copy of `a` via indirect dependency on `text` via `webutil`.
We see neither the second indirect copy of `a` nor the indirect copy of `x` via webutil because we already have names for them.
``` ucm
scratch/app2> names a
scratch/app2> names x
```
