Installing the libraries your code depends on. A library is a project from [Unison Share](learn:ucm/share) installed under the namespace `lib` of your own [project](learn:ucm/branches).

- To find a library for a job, search Share (`share.project-search json`) and read its README before installing it: see [libraries from Share](learn:skills/library).
- `lib.install @user/project/releases/latest` installs the latest release of a library from Unison Share into `lib`. Also `@user/project/releases/1.2.3`, or `@user/project/branch`.
- The folder under `lib` is named for the library and version installed, so `list lib` or the output of `lib.install` shows the name to use, like `lib.unison_base_7_20_0.data.List.map`.
- To see what is installed, run `list lib` (it works with every tool, even `transcript-read-only`). Each line is a library folder, named for the library and its version, like `unison_base_7_13_1` for `@unison/base` 7.13.1. Check it before installing, so you don't add a second copy of a library you already have.
- `find.all <name>` searches libraries as well as your own code, and `view lib.<folder>.<name>` shows a library definition (see [find](learn:ucm/find)).
- To move to a newer version, see [`upgrade`](learn:ucm/upgrade).

## Detail

**Learning a library without installing it.** Find it on [Unison Share](learn:ucm/share), then read its README and documentation with `share.render`, and a function's source with `share.source`.

**What `list lib` shows.** In a project with several libraries, each folder is a library at a version, and the counts are the definitions in it:

````
``` ucm
myproject/main> list lib

  1.  ansi_1_0_0.          (6511 terms, 157 types)
  2.  json_1_2_1.          (6651 terms, 162 types)
  3.  unison_base_7_13_1.  (8732 terms, 191 types)
  4.  unison_cloud_27_2_1. (113093 terms, 2687 types)
  5.  unison_cloud_27_3_0. (113071 terms, 2688 types)
```
````

The same library can be there at two versions, as `unison_cloud` is here. `legacy.` is a folder for old code that has no library name, not something you installed. Reading a library's definitions by their full names (`view lib.unison_base_7_13_1.data.List.map`) shows that version's code.

**Libraries of libraries.** A library can have its own `lib`. `find.all` includes one level of it.
