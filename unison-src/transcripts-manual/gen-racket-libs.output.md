
When we start out, `./scheme-libs/racket` contains a bunch of library files that we'll need. They define the Unison builtins for Racket.

Next, we'll download the jit project and generate a few Racket files from it.

```ucm
.> project.create-empty jit-setup

  🎉 I've created the project jit-setup.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

jit-setup/main> pull @unison/internal/releases/0.0.13 lib.jit

  Downloaded 15053 entities.

  ✅
  
  Successfully pulled into lib.jit, which was empty.

```
```unison
go = generateSchemeBoot "scheme-libs/racket"
```

```ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      go : '{IO, Exception} ()

```
```ucm
jit-setup/main> run go

  ()

```
After executing this, `scheme-libs/racket` will contain the full
complement of unison libraries for a given combination of ucm version
and @unison/internal version.

To set up racket to use these files, we need to create a package with
them. This is accomplished by running.

    raco pkg install -t dir unison

in the directory where the `unison directory is located. Then the
runtime executable can be built with

    raco exe scheme-libs/racket/unison-runtime.rkt

and a distributable directory can be produced with:

    raco distribute <output-dir> scheme-libs/racket/unison-runtime

At that point, <output-dir> should contain the executable and all
dependencies necessary to run it.
