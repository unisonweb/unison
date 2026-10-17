# Building on Windows

## I get an error about unison/sql/something

This codebase uses symlinks as a workaround for some inconveniences in the `here` package. Support for symlinks in Windows is relatively new, and isn't enabled by default. As a result, your cloned copy of the code probably won't build.

First you'll need to enable "Developer Mode" in your Windows settings.

> See https://consumer.huawei.com/en/support/content/en-us15594140/

Then you'll need to enable symlink support in your `git` configuration, e.g.

```shell
git config core.symlinks true
```

And then ask `git` to fix up your symlinks with `git checkout .`

More context at: https://stackoverflow.com/a/59761201/310162

## I get an error about `removeDirectoryRecursive`/`removeContentsRecursive`/`removePathRecursive`/`permission denied (Access is denied.)`

Stack doesn't work deterministically in Windows due to mismatched expectations about how file deletion works. If you get this error, you can just retry the build and it will probably make more progress than the last time.
