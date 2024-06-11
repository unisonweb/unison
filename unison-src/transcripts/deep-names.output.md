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

  ⚠️
  
  The namespace .text doesn't exist.

```

```ucm
.app1> fork .text lib.text_v1.app1> fork .text lib.text_v2.app1> fork .http lib.http_v3.app1> fork .http lib.http_v4
```


🛑

The transcript failed due to an error in the stanza above. The error is:


  ⚠️
  
  The namespace .text doesn't exist.

