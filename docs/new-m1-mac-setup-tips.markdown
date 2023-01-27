
# M1 Mac Haskell toolchain setup

If you are a newcomer to the Haskell ecosystem trying to set up your dev environment on a Mac M1 computer, welcome, you can do this! The tips in this document provide one way to get a working development setup, but are not the only path forward. If you haven't downloaded the Haskell toolchain before, our recommendation is to use GHCup. If you're a veteran Haskell developer, much of this won't apply to you as it's likely you already have a working development environment.

Here are the versions you'll need to build the Unison executable (as of January 24th, 2023)

GHC version: 8.10.7
Stack version: 2.7.5
Cabal version 3.6.2.0
Haskell language server version: 1.7.0.0

## Newcomer setup tips

[Install GHCup using the instructions on their website.](https://www.haskell.org/ghcup/) Once it's isnstalled make sure `ghcup` is on your path.

```
export PATH="$HOME/.ghcup/bin:$PATH"
```

GHCup has a nice ui for setting Haskell toolchain versions for the project. Enter `ghcup tui` to open it up and follow the instructions for installing and setting the versions there. GHCup will try to download M1 native binaries for the versions given.

Check your clang version. For [hand-wavey reasons](https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/301) we recommend you use llvm version 12.

```shell
$ clang --version
Homebrew clang version 12.0.1
Target: arm64-apple-darwin20.2.0
Thread model: posix
InstalledDir: /opt/homebrew/opt/llvm@12/bin
```

At the end of the process you should see something like the following for executable locations and versions.

```shell
$ which ghcup
~/.ghcup/bin/ghcup
$ ghcup --version
The GHCup Haskell installer, version 0.1.19.0
```

```bash
$ which stack
~/.ghcup/bin/stack
$ stack --version
Version 2.7.5, Git revision 717ec96c15520748f3fcee00f72504ddccaa30b5 (dirty) (163 commits) aarch64
```

```shell
$ which ghc
~/.ghcup/bin/ghc
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
```

Check which GHC version Stack thinks it's using too, for good measure:

```shell
$ stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
$ stack exec -- which ghc
~/.ghcup/ghc/8.10.7/bin/ghc
```

```shell
$ which haskell-language-server-wrapper
~/.ghcup/bin/haskell-language-server-wrapper
$ haskell-language-server-wrapper

Found "...unison/hie.yaml" for "...unison/a"
Run entered for haskell-language-server-wrapper(haskell-language-server-wrapper) Version 1.7.0.0 aarch64 ghc-9.2.2
Current directory: ...unison
Operating system: darwin
Arguments: []
Cradle directory: ...unison
Cradle type: Stack

Tool versions found on the $PATH
cabal:		3.6.2.0
stack:		2.7.5
ghc:		8.10.7
```

If you're a VS Code user, you can download the Haskell extension for IDE support. You may need to configure it in `settings.json`.

```json
    "haskell.manageHLS": "GHCup",
    "haskell.toolchain": {
      "stack": "2.7.5",
      "ghc": "8.10.7",
      "cabal": "recommended",
      "hls": "1.7.0.0"
    }
```

These setting blocks say that the VS Code extension will use GHCup for your Haskell language server distribution, and sets the versions for elements in the toolchain.

## Troubleshooting:

The VS Code extension has compiled a helpful list of troubleshooting steps here: https://github.com/haskell/vscode-haskell#troubleshooting

### "Couldn't figure out LLVM version" or "failed to compile a sanity check" errors

```
<no location info>: error:
    Warning: Couldn't figure out LLVM version!
             Make sure you have installed LLVM between [9 and 13)
ghc: could not execute: opt
```

Or

```
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
`gcc' failed in phase `Linker'. (Exit code: 1)
```

Try installing llvm version 12
`brew install llvm@12`

and prepend it to your path
```
export PATH="$(brew --prefix)/opt/llvm@12/bin:$PATH"
```

(The GHC version 8.10.7 mentions it supports LLVM versions up to 12. https://www.haskell.org/ghc/download_ghc_8_10_7.html)

### "GHC ABIs don't match!"

Follow the steps here:

https://github.com/haskell/vscode-haskell#ghc-abis-dont-match

We found some success telling Stack to use the system's GHC instead of managing its own version of GHC. You can try this by setting the following two configuration flags in ~/.stack/config.yaml

```
system-ghc: true
install-ghc: false
```

This is telling Stack to use the GHC executable that it finds on your $PATH. Make sure the ghc being provided is the proper version, 8.10.7, from ghcup.

Note that you may need to clean the cache for the project after this failure with `stack clean --full` if you have previously built things with a different stack distribution.

### "stack" commands like "stack build" cause a segfault:

1. Make sure your stack state is clean. `stack clean --full` removes the project's stack work directories (things in .stack-work).
2. [Wait for this bug to be fixed (or help fix this bug!)](https://github.com/commercialhaskell/stack/issues/5607)
3. Or Subshell out your stack commands $(stack blahblah)
4. Or use bash instead of zsh

### Help! Everything is broken and I want to start over

Warning, the following will remove ghcup, configuration files, cached packages, and versions of the toolchain.

```
ghcup nuke
rm -rf ~/.ghcup
rm -rf ~/.stack
rm -rf ~/.cabal
```
