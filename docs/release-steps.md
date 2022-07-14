
__0__

Communicate with core team - we are cutting a release now, are there any showstopping bugs that need fixing first?

__1__

Create and push the tag to github. This will trigger the build. To determine the last release, check [the releases page](https://github.com/unisonweb/unison/releases).

```
git fetch
git checkout series/M2
git merge origin/trunk
git tag -a release/$RELEASE_NAME -m "release"
git push origin release/$RELEASE_NAME
git push origin series/M2
```

__2__

Wait for the release to show up on [the releases page](https://github.com/unisonweb/unison/releases). This can take an hour or two!

__3__

Create a release notes draft issue, following [this template](https://github.com/unisonweb/unison/issues/2342) and updating the output of PRs merged and contributors to the release.

__4__

Update trunk of `base` to include any new builtins added since last release. Suggestion for how to do this: look through the release notes draft to find the PRs merged since last release. @runarorama does this usually.

```
git log --oneline release/M2h...release/M2i | grep 'Merge pull request #'
```

Then just use `alias.term ##Nat.newBuiltin Nat.someName` and/or `alias.type ##SomeType SomeType`. I think this is probably better than doing `builtins.merge` at this point.

__5__

Cut a release of base. @runarorama does this usually.


```
.> pull git(git@github.com:unisonweb/base) basedev.release
.> cd .basedev.release
.basedev.release> delete.namespace releases._latest
.basedev.release> squash trunk releases._<ReleaseName>
```

Edit `releases._<ReleaseName>.README` to include `Release: <ReleaseName>`.

```
.basedev.release> fork releases._<ReleaseName> releases._latest
.basedev.release> push git(git@github.com:unisonweb/base)
```

__6__

Mark a release of the [Codebase UI](https://github.com/unisonweb/codebase-ui) with a matching version number to that of the UCM release.
Compile a UI Changelog for the release notes from the Done column on the [Codebase UI Project](https://github.com/unisonweb/codebase-ui/projects/2)

__7__

Build a new version of Unison Share by following these instructions: https://github.com/unisonweb/share#for-share-codebase-maintainers

__8__

Update homebrew.

```
git clone git@github.com/unisonweb/homebrew-unison.git
```

Update this file: https://github.com/unisonweb/homebrew-unison/blob/master/unison-language.rb and change the version number and the path to the release tar files.

To get the updated sha256 values, use the following command, replacing the download link with the linux and mac downloads respectively.

```sh
curl -sSL https://github.com/unisonweb/unison/releases/download/release%2FM2h/ucm-linux.tar.gz | shasum -a 256 | cut -f1 -d" "
```

__9__

Announce on #general Slack channel. Template below.

---

Release announcement template (be sure to update the release urls) -

We've just released a new version of Unison, $RELEASE_NAME, release notes here (link to the issue). Install/upgrade instructions in the thread.

Mac upgrade is just `brew upgrade unison-language`.

A fresh install via:

```
brew tap unisonweb/unison
brew install unison-language
```

If you have previously done brew install unison-language --head to install a dev build, uninstall that first via brew uninstall unison-language.

_Linux manual install:_

```
mkdir unisonlanguage
curl -L https://github.com/unisonweb/unison/releases/download/release%2FM2h/ucm-linux.tar.gz --output unisonlanguage/ucm.tar.gz
tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage
./unisonlanguage/ucm
```

_Mac manual install:_

```
mkdir unisonlanguage
curl -L https://github.com/unisonweb/unison/releases/download/release%2FM2h/ucm-macos.tar.gz --output unisonlanguage/ucm.tar.gz
tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage
./unisonlanguage/ucm
```
