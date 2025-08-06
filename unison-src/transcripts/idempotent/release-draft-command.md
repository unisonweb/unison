The `release.draft` command drafts a release from the current branch.

``` ucm :hide
> builtins.merge
```

Some setup:

``` unison
someterm = 18
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + someterm : Nat

  Run `update` to apply these changes to your codebase.
```

``` ucm
> add

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.
```

Now, the `release.draft` demo:

`release.draft` accepts a single semver argument.

``` ucm
> release.draft 1.2.3

  😎 Great! I've created a draft release for you at
  /releases/drafts/1.2.3.

  You can create a `ReleaseNotes : Doc` in this branch to give
  an overview of the release. It'll automatically show up on
  Unison Share when you publish.

  When ready to release 1.2.3 to the world, `push` the release
  to Unison Share, navigate to the release, and click "Publish".

  Tip: if you get pulled away from drafting your release, you
       can always get back to it with
       `switch /releases/drafts/1.2.3`.
```

It's an error to try to create a `releases/drafts/x.y.z` branch that already exists.

``` ucm :error
> release.draft 1.2.3

  scratch/releases/drafts/1.2.3 already exists. You can switch
  to it with `switch scratch/releases/drafts/1.2.3`.
```
