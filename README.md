The Unison platform
======

[![Join the chat at https://gitter.im/unisonweb/platform](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/unisonweb/platform?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[Unison](http://unisonweb.org) is a new programming platform, currently under active development. This repo contains the code for the Unison node backend (written in Haskell, lives in the `node` directory, with source in `src`), and the Unison editor (currently written in Elm, found in the folder `editor-elm`).

If you're wondering what the project is about, you can get a glimpse [with this video and post discussing the Unison semantic editor](http://pchiusano.github.io/2015-03-17/unison-update5.html). The editor is just one piece of the overall platform, and  there's updates and more info at [unisonweb.org](http://unisonweb.org) as well as [background posts with additional context](http://pchiusano.io/unison).

Since Unison isn't terribly useful in its current form, the rest of this README will focus on stuff that will be of interest for potential contributors, namely, how to build the code, and a brief tour of the (fairly small but action-packed) codebase. If you're just interested in the project and want to follow along with the progress, [unisonweb.org](http://unisonweb.org) is the place to go, or you can also say hello or lurk [in the chat room](https://gitter.im/unisonweb/platform).

Still here? All right then! Let's get to it.

A brief code tour
-----

First, a bit of orientation. Here's the directory structure:

```
shared/
node/
editor/
editor-elm/
```

The `editor-elm/` directory is the current Elm implementation of the Unison editor. It's being phased out, in favor of a Unison editor written in Haskell and compiled to Javascript via [GHCJS](https://github.com/ghcjs/ghcjs). By writing the editor in Haskell, we can share code between the editor and Unison node backend. So what's with the directory structure? . The `shared/` directory has Haskell code that will be shared between the editor and node, the `node/` directory has code specific to the node, and `editor/` is the currently-in-development Haskell version of the editor, which also depends on `shared/`.

The dependencies are what you'd expect---`shared/` has minimal external dependencies, and `node/` and `editor/` depend on `shared`. Thus, it should be very obvious and explicit what code and external dependencies are going to be compiled to JS.

Build instructions
-----

A couple notes before getting started:

* If you're on Windows or just prefer to build the code on a known-good VM, use the [Vagrant box setup](#vagrant) after reading through these instructions.
* If you're on NixOS or just prefer to build all dependencies from source, see the [note below about trusted binary caches](#binary-caches).

### Step 1: Install Nix

The only thing one needs to install beforehand is the [Nix package manager](https://nixos.org/nix/). You don't need to install Haskell, cabal, or anything else. Nix will build the Unison node, editor, and all their dependencies (including GHC), with dependencies pinned by our Nix configuration to "known good" versions. You do not need to have Haskell or cabal installed in advance!

_Note: For people nervous about installing Nix, Nix keeps all packages and binaries in its own directory (generally `/nix`) and the only thing it adds to your path are some commands like `nix-shell`, `nix-build`, and so on, so it won't interfere with any other setup you have on your machine._

The easiest way to install Nix is:

```sh
$ curl https://nixos.org/nix/install | sh
```

_Note: If you are alarmed at the suggestion of [executing unknown scripts from the internet](http://curlpipesh.tumblr.com/), see the other install options [on the Nix website](https://nixos.org/nix)._

### <a id="step-2"></a> Step 2: Build the Unison node

After install completes, you'll see instructions about starting a fresh terminal session. Do that, then:

```sh
$ git clone https://github.com/unisonweb/platform.git unisonweb
$ cd unisonweb
$ cd node
$ ./shell.sh
```

The first time you run this, it will take a few minutes as Nix needs to download and/or build dependencies, and you may see some warnings about Haddock documentation that you can ignore. Subsequent launches will be snappy since you'll have all the dependencies in your Nix store.

Once this completes, you'll be in a Nix shell with cabal (the Haskell build tool) on your path, properly configured. You can just use it as normal. For instance:

```sh
cabal build # compile the code
cabal repl  # start a repl with access to the project
cabal test  # run the tests
cabal run   # launch the node server
```

If you do `cabal run`, you'll see a message like:

```sh
Running node...
Setting phasers to stun... (port 8080) (ctrl-c to quit)
```

That message is [Scotty](http://hackage.haskell.org/package/scotty) telling you it's running. That means you're good.

### Step 3: Build the Unison editor

Just do:

```
$ cd editor
$ ./shell.sh
```

You'll again be put into a shell with access to cabal. The editor uses GHCJS and not all cabal commands are supported, but `cabal build` will compile the Haskell code to JS. Just open the following file in a browser:

```
editor/dist/build/editor/editor.jsexe/index.html
```

Note: at the moment, the editor is just "Hello World". Work on the editor is currently ongoing! Check back in a couple months. If you like, [here are instructions for building the legacy Elm-based Unison editor](https://github.com/unisonweb/platform/blob/master/editor-elm/README.md).

A brief code tour of the Haskell code
-----

The Unison Haskell code, which has the language, its typechecker, and the node implementation, is split between `shared/` and `node/`. It's not actually much code right now, only about 3k lines!

Obviously, this number is going to go up over time, but right now, it's pretty bite-sized and (hopefully) easy enough to follow. Certainly not of the scale of something like GHC, which clocks in at [over 135k LOC](http://www.aosabook.org/en/ghc.html)!

One brief note for orientation. Because of the split between `shared/` and `node/`, a module like `Unison.Term` (in `shared/`), which has the basic type and instances for JSON encoding/decoding, has a counterpart in `shared/`, `Unison.Term.Extra` with things like _binary_ serialization or hashing of `Unison.Term` values. Logically, it would be nice to put all functionality in the `Unison.Term` module, but binary serialization and hashing code isn't needed by the editor and we don't want to accidentally compile those libraries and code to JS or rely on tree-shaking to hopefully trim it out. Other `.Extra` modules are analogous.

Now, where to begin? Everyone learns differently. You might prefer to read the code 'inside out' (or perhaps 'bottom up'), starting from the core language syntax tree and typechecker, then expanding out to where these get exposed to the outside world. If this route sounds appealing, here's a reasonable path:

* `Unison.Term` in `shared/` is the module containing the definition for Unison language _terms_ and `Unison.Type` is the module containing the definition for Unison language _types_. Eventually, we'll add `Unison.TypeDeclaration`.
* In both `Term` and `Type`, the same pattern is used. Each defines a 'base functor' type, `F a`, which is nonrecursive, and the actual thing we use is an _abstract binding tree_ over this base functor, an `ABT F`. `ABT` (for 'abstract binding tree') is defined in `Unison.ABT`. If you aren't familiar with abstract binding trees, [here is a nice blog post explaining one formulation of the idea](http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html), which inspired the `Unison.ABT` module. A lot of operations on terms and types just delegate to generic `ABT` operations. Also see `Unison.ABT.Extra`.
* The main interface to the typechecker is in `node/` in `Unison.Typechecker`, and the implementation is in `Unison.Typechecker.Context`. There isn't a lot of code here (about 500 LOC presently), since the typechecking algorithm is pretty simple. Unlike a unification-based typechecker, where the typechecking state is an unordered bag of unification constraints and higher-rank polymorphism is usually bolted on awkwardly later, [Dunfield and Krishnaswami's algorithm](http://www.mpi-sws.org/~neelk/bidir.pdf) keeps the typechecking state as a nicely tidy _ordered context_, represented as a regular list manipulated in a stack-like fashion, and the algorithm handles higher-rank polymorphism very cleanly. They've also [extended this work to include features like GADTs](http://semantic-domain.blogspot.com/2015/03/new-draft-sound-and-complete.html), though this new algorithm hasn't been incorporated into Unison yet.
* From here, you can move to `Unison.Node`, which defines the interface satisfied by the node, `Unison.Node.Implementation`, containing a simple implementation of that interface, and `Unison.NodeServer`, which just wraps the node API in an HTTP+JSON interface.
* Lastly, `node/src/Node.hs` has the code which creates an instance of a `Unison.NodeServer`. The `src/node/Node.hs` file also has the definition of the current Unison 'standard library'. The node logic is agnostic to the "standard library" chosen, so whatever creates an instance of `Unison.Node` has to supply it with the standard library it should use.

If instead, you'd rather work from the 'outside in' (or perhaps 'top down'), you could start with `Unison.NodeServer` and work your way back the other direction to modules like `Term`, `Type`, and `ABT`. Since the entire point of the node codebase is to expose an API over HTTP, `Unison.NodeServer` will end up referencing directly or indirectly all the code in the node, and all the Unison language and typechecker.

That's all for now!

### <a id="vagrant"></a> Appendix: Build instructions for Windows users or those who prefer to build code on a VM

NOTE: these instructions don't work just yet.

If you're on Windows and would like to build the project, you can do so using the Vagrant box VM. You can also do this if you just prefer to develop using a VM. If you do this, you can still use your local text editor or IDE of choice for Haskell editing, since the filesystem is shared between the VM and your local machine.

Here are instructions for this route:

* Download and install [Vagrant](https://www.vagrantup.com/).
* Download and install [VirtualBox](https://www.virtualbox.org/). This is a free VM provider. If you like, you can also pay for [Vagrant+VMWare](https://www.vagrantup.com/vmware), which is supposed to be higher performance.

Once those are done, from the root directory of the project (the same directory as the `Vagrantfile` file), do:

```sh
$ vagrant up
... lots of log output as the machine gets set up
```

Note that depending on your terminal, the log output may be very badly formatted, with carriage returns getting interpreted as newlines. Don't worry about that. Once it completes, you can do:

```
$ vagrant ssh
Welcome to Ubuntu 14.04.2 LTS (GNU/Linux 3.13.0-55-generic x86_64)

 * Documentation:  https://help.ubuntu.com/

 System information disabled due to load higher than 1.0

  Get cloud support with Ubuntu Advantage Cloud Guest:
    http://www.ubuntu.com/business/services/cloud

0 packages can be updated.
0 updates are security updates.


vagrant@vagrant-ubuntu-trusty-64:~$ cd /vagrant/
vagrant@vagrant-ubuntu-trusty-64:/vagrant$ ls
build-and-run  editor-elm  node         shell-common.nix
dist           env.nix     README.md    Vagrantfile
editor         LICENSE     shared       vagrant-provision.sh
```

Notice that the `/vagrant` directory on the VM mirrors the root directory of your project. You can edit the code on your local machine, and use the the [usual build instructions on the VM](#step-2) to compile and run the project on the VM!

### <a id="binary-caches"></a> Appendix: Build instructions for NixOS users or those who prefer to build all dependencies from source

Rather than always building from source, Nix will use binary caches for packages you request, if they are available in a 'trusted' cache.

If you're running NixOS, the `shell.sh` scripts won't work, since these scripts supply the additional binary caches via command line flags (which NixOS disallows):

```sh
$ cat node/shell.sh
#!/bin/sh
nix-shell --option extra-binary-caches https://ryantrinkle.com:5443/ -j 8

$ cat editor/shell.sh
#!/bin/sh
nix-shell --option extra-binary-caches https://ryantrinkle.com:5443/ -j 8 -A ghcjs
```

You can do one of two things:

* Just leave off the `--option extra-binary-caches https://ryantrinkle.com:5443` flag. This should still work, but will take a long time (several hours) the first time you do it, since it will be building a huge number of dependencies from source. You can also do this if you prefer to build all dependencies from source.
* Or, edit your Nix configuration at `/etc/nix/configuration.nix` to add the following binary caches:
  * https://ryantrinkle.com:5443/
  * http://zalora-public-nix-cache.s3-website-ap-southeast-1.amazonaws.com/

If you're on an OS other than NixOS, you can also add binary caches to your Nix configuration to avoid having to specify them on the command line like the `shell.sh` files do. To do this, edit your `nix.conf` file (it may not exist yet, in which case create `/nix/nix.conf` or `/etc/nix/nix.conf` dependending on where Nix is installed on your platform) and add the following line:

```
binary-caches = https://ryantrinkle.com:5443 http://zalora-public-nix-cache.s3-website-ap-southeast-1.amazonaws.com/
```

The `http://zalora-public-nix-cache.s3-website-ap-southeast-1.amazonaws.com/` has Darwin binaries and `https://ryantrinkle.com:5443` has some of editor's dependencies on all platforms. You can add more caches separated by spaces if you like.
