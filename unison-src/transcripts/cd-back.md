## Switching between namespaces / projects / branches / modules

Unison uses the same organizational element to represent directories, projects, sub-projects, forks, modules, etc.; currently called a "namespace".

Namespaces are trees that contain definitions of "types" and "terms", "patches", and other child namespaces.

We're still working out what a nice codebase layout might be (feel free to write up a blog post if you find one that works well for you), but in this example, we have these, along with their children (not shown):

> .libs.base
> .libs.megaparser.master
> .libs.megaparser.v1
> .libs.megaparser.v2
> .arya.base
> .arya.myproject
> .pullrequests.runarorama.base_3.base
> .pullrequests.runarorama.base_3.head
> .pullrequests.runarorama.base_3.merged
> .temp

```ucm:hide
.> builtins.merge
.> move.namespace builtin .arya.base
```

```ucm
.> cd arya.base
.arya.base> find Boolean
```
```ucm:hide
.arya.base> cd .arya.myproject
```

blah blah blah more stuff about project management and patches and the value of working from the appropriate namespace, and what that is in any given case

We can pop back to the previous namespace with the `back` command.

```ucm:hide
.arya.myproject> back
```
```ucm:hide
.arya.base> back
```
```ucm:error
.> back
```
😬 Right, ok.
