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

