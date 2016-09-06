The Unison platform
======

[![Join the chat at https://gitter.im/unisonweb/platform](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/unisonweb/platform?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/unisonweb/unison.svg?branch=master)](https://travis-ci.org/unisonweb/unison)

[Unison](http://unisonweb.org) is a new programming platform, currently under active development. This repo contains the code for the Unison node backend (written in Haskell, lives in the `node` directory, with source in `src`), and the Unison editor.

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

The `shared/` directory has Haskell code that will be shared between the editor and node, the `node/` directory has code specific to the node, and `editor/` is the Unison editor, which also depends on `shared/` but is compiled to Javascript via [GHCJS](https://github.com/ghcjs/ghcjs). By writing the editor in Haskell, we can share code between the editor and Unison node backend. The `editor-elm/` directory is an older Elm implementation of the Unison editor that is being phased out.

The dependencies are what you'd expect---`shared/` has minimal external dependencies, and `node/` and `editor/` depend on `shared`. Thus, it should be very obvious and explicit what code and external dependencies are going to be compiled to JS.

Building with Docker
-----
The easiest way to compile Unison yourself is to use the provided Dockerfile.
If you have [Docker set up correctly](https://docs.docker.com/engine/installation/),
just run
```sh
$ git clone https://github.com/unisonweb/unison.git unisonweb
$ cd unisonweb
$ docker build -t unisonweb/platform .
```
which will take quite a while (~1h) the first time. It's much speedier on subsequent runs.
When the above command finishes you can start Unison with
```sh
$ docker run -it -p 8080:8080 --name unisonweb --rm unisonweb/platform
Setting phasers to stun... (port 8080) (ctrl-c to quit)
```
and then browse to [http://localhost:8080](http://localhost:8080) to open the editor.
On Mac and Windows replace `localhost` by the IP of your Docker VM.

<a name="stackbuild"></a>Building using Stack
-----

If these instructions don't work for you or are incomplete, please file an issue.
Also, have a look at the Dockerfile if you are unsure about the steps to perform.

The build uses [Stack](http://docs.haskellstack.org/). If you don't already have it installed, version 1.0.2 or later, [follow the install instructions](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform. Once that's done and the `stack` executable is on your path, do:

```sh
$ git clone https://github.com/unisonweb/unison.git
$ cd unison
$ sudo apt-get install libcurl4-openssl-dev
$ stack --version # make sure this returns 1.0.2 or later
$ stack setup
$ stack build unison-node # build node executable
```

To build the editor, do:
```sh
$ sudo apt-get install nodejs
$ stack --stack-yaml editor.yaml setup # first time only
$ stack --stack-yaml editor.yaml build
```

The editor is built using GHCJS. If you encounter an issue about missing 'happy', or "'ghc' is required but it could not be found" while building 'happy', you can try installing with `stack install happy`, `cabal install happy` or `sudo apt-get install happy` [if on ubuntu](#ubuntu-editor-issues). If you're using the Vagrant VM then `sudo apt-get install cabal-install; cabal update; cabal install happy` works.  Make sure that `happy` ends up on your `$PATH` (try doing `happy --version`; it should report 1.19.5 or later) after install.  

_After_ `stack build` completes successfully, you can symlink the generated Javascript files by performing a

```sh
$ ln -s $(stack --stack-yaml editor.yaml path --local-install-root)/bin editor
```

You can run it by doing:

```sh
$ stack exec node
Setting phasers to stun... (port 8080) (ctrl-c to quit)
```

That last message is [Scotty](http://hackage.haskell.org/package/scotty) telling you it's running. That means you're good. Visit <http://localhost:8080/> in a browser to see the editor (or just open up `editor/editor.html`).  You can take a look at the posts [here](http://unisonweb.org/editor) for clues on how the editor is used!

These instructions do not work on Windows as far as I know (this might be fixable, contact me if interested), but if you're on Windows or just prefer to build the code on a known-good VM, use the [Vagrant box setup](#vagrant) after reading through these instructions. If you go this route, you can still use your preferred text editor. The VM will have shared filesystem access to the directory where you've checked out the code.

A brief tour of the Haskell code
-----

The Unison Haskell code, which has the language, its typechecker, and the node implementation, is split between `shared/` and `node/`. It's not actually much code right now, only about 3k lines!

Obviously, this number is going to go up over time, but right now, it's pretty bite-sized and (hopefully) easy enough to follow. Certainly not of the scale of something like GHC, which clocks in at [over 135k LOC](http://www.aosabook.org/en/ghc.html)!

One brief note for orientation. Because of the split between `shared/` and `node/`, a module like `Unison.Term` (in `shared/`), which has the basic type and instances for JSON encoding/decoding, has a counterpart in `node/`, `Unison.Term.Extra` with things like _binary_ serialization or hashing of `Unison.Term` values. Logically, it would be nice to put all functionality in the `Unison.Term` module, but binary serialization and hashing code isn't needed by the editor and we don't want to accidentally compile those libraries and code to JS or rely on tree-shaking to hopefully trim it out. Other `.Extra` modules are analogous.

Now, where to begin? Everyone learns differently. You might prefer to read the code 'inside out' (or perhaps 'bottom up'), starting from the core language syntax tree and typechecker, then expanding out to where these get exposed to the outside world. If this route sounds appealing, here's a reasonable path:

* `Unison.Term` in `shared/` is the module containing the definition for Unison language _terms_ and `Unison.Type` is the module containing the definition for Unison language _types_. Eventually, we'll add `Unison.TypeDeclaration`.
* In both `Term` and `Type`, the same pattern is used. Each defines a 'base functor' type, `F a`, which is nonrecursive, and the actual thing we use is an _abstract binding tree_ over this base functor, an `ABT F`. `ABT` (for 'abstract binding tree') is defined in `Unison.ABT`. If you aren't familiar with abstract binding trees, [here is a nice blog post explaining one formulation of the idea](http://semantic-domain.blogspot.com/2015/03/abstract-binding-trees.html), which inspired the `Unison.ABT` module. A lot of operations on terms and types just delegate to generic `ABT` operations. Also see `Unison.ABT.Extra`.
* The main interface to the typechecker is in `node/` in `Unison.Typechecker`, and the implementation is in `Unison.Typechecker.Context`. There isn't a lot of code here (about 500 LOC presently), since the typechecking algorithm is pretty simple. Unlike a unification-based typechecker, where the typechecking state is an unordered bag of unification constraints and higher-rank polymorphism is usually bolted on awkwardly later, [Dunfield and Krishnaswami's algorithm](http://www.mpi-sws.org/~neelk/bidir.pdf) keeps the typechecking state as a nicely tidy _ordered context_, represented as a regular list manipulated in a stack-like fashion, and the algorithm handles higher-rank polymorphism very cleanly. They've also [extended this work to include features like GADTs](http://semantic-domain.blogspot.com/2015/03/new-draft-sound-and-complete.html), though this new algorithm hasn't been incorporated into Unison yet.
* From here, you can move to `Unison.Node`, which defines the interface satisfied by the node, `Unison.Node.Implementation`, containing a simple implementation of that interface, and `Unison.NodeServer`, which just wraps the node API in an HTTP+JSON interface.
* Lastly, `node/src/Node.hs` has the code which creates an instance of a `Unison.NodeServer`. The `src/node/Node.hs` file also has the definition of the current Unison 'standard library'. The node logic is agnostic to the "standard library" chosen, so whatever creates an instance of `Unison.Node` has to supply it with the standard library it should use.

If instead, you'd rather work from the 'outside in' (or perhaps 'top down'), you could start with `Unison.NodeServer` and work your way back the other direction to modules like `Term`, `Type`, and `ABT`. Since the entire point of the node codebase is to expose an API over HTTP, `Unison.NodeServer` will end up referencing directly or indirectly all the code in the node, and all the Unison language and typechecker.

That's all for now!

### Appendix

#### <a id="vagrant"></a>Build instructions for Windows users or those who prefer to build code on a VM

If you're on Windows and would like to build the project, you can do so using the Vagrant box VM. You can also do this if you just prefer to develop using a VM. If you do this, you can still use your local text editor or IDE of choice for Haskell editing, since the filesystem is shared between the VM and your local machine.

Here are instructions for this route:

* [Clone](https://help.github.com/articles/cloning-a-repository/) the unison repo (`git clone https://github.com/unisonweb/platform.git unisonweb`).
* Download and install [Vagrant](https://www.vagrantup.com/).
* Download and install [VirtualBox](https://www.virtualbox.org/). This is a free VM provider.

Once those are done, from the root directory of the project (the same directory as the `Vagrantfile` file), and from a Windows shell with admin privileges (right-click, 'Run as administrator'), do:

```
> vagrant up
... lots of log output as the machine gets set up
```

Once it completes, you can do `vagrant ssh`, then `cd /vagrant`. Notice that the `/vagrant` directory on the VM mirrors the root directory of your project. You can edit the code on your local machine, and follow the usual ['building using stack'](#stackbuild) instructions on the VM to compile and run the project on the VM!  

The `Vagrantfile` configures the VM with 3GB RAM.  If you don't have that much to spare then you might hit out-of-memory problems as you try to build Unison within the VM.  In that case you can cut down the RAM allocation by editing the Vagrantfile (and then doing `vagrant reload`), but you'll need to make up the difference by logging in to the VM (`vagrant ssh`) and configuring swap space [like so](https://www.digitalocean.com/community/tutorials/how-to-add-swap-on-ubuntu-14-04) - but be warned your builds will be a good deal slower.  

#### <a id="ubuntu-editor-issues"></a>Problems Building the Editor - Ubuntu

At least one user has reported problems when building the editor on a machine running Ubuntu 14.04.

1. If you encounter the following error: `Link error: “Cannot find -ltinfo”`, then you'll need to install the `libtinfo-dev` package with:
    ```sh
    $ sudo apt-get install libtinfo-dev
    ```

1. If you encounter an error like the following:
    ```
    Unpacking GHCJS into <some-directory> ...Expected a single directory within unpacked <some-tar.gz-file>"
    ```

    then you need to upgrade `stack` to at least version 1.0.5.

1. If you get an error that contains:
    ```
    The program 'happy' version >=1.17 is required but it could not be found.
    ```

    then you'll need to install `happy`.  [This](https://github.com/commercialhaskell/stack/issues/595) `stack` bug seems to make it impossible to get `stack` to build `happy`.  If this is the case for you, you should be able to install using apt:
    ```sh
    $ sudo apt-get install happy
    ```
	
	or else try 'sudo apt-get install cabal-install; cabal update; cabal install happy'.
