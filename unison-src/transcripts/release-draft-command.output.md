The `release.draft` command drafts a release from the current branch.

Some setup:

```unison
someterm = 18
```

```ucm

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      someterm : Nat

```
```ucm
.> project.create foo

  I just created project foo with branch main.

foo/main> add

  ⍟ I've added these definitions:
  
    someterm : Nat

```
Now, the `release.draft` demo:

`release.draft` accepts a single semver argument.

```ucm
foo/main> release.draft 1.2.3

  😎 Great! I've created a draft release for you at
  /releases/drafts/1.2.3.
  
  While preparing we suggest you create a releaseNotes Doc.
  It'll automatically show up on Unison Share when you publish.
  
  When ready to release 1.2.3 to the world, `push` the release
  to Unison Share, navigate to the release, and click "Publish".
  
  Tip: if you get pulled away from drafting your release, you
       can always get back to it with
       `switch /releases/drafts/1.2.3` .

```
It's an error to try to create a `releases/drafts/x.y.z` branch that already exists.

```ucm
foo/main> release.draft 1.2.3

  foo/releases/drafts/1.2.3 already exists.

```
