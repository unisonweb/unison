The Unison language
======

[![Build Status](https://travis-ci.org/unisonweb/unison.svg?branch=master)](https://travis-ci.org/unisonweb/unison)

[Unison](https://unisonweb.org) is a new programming language, currently under active development. It's a modern, statically-typed purely functional language, similar to Haskell, but with the ability to describe entire distributed systems with a single program. Here's an example of a distributed map-reduce implementation:

```Haskell
-- comments start with `--`
mapReduce loc fn ifEmpty reduce data = match split data with
  Empty          -> ifEmpty
  One a          -> fn a
  Two left right ->
    fl = at loc '(mapReduce loc fn ifEmpty reduce !left)
    fr = at loc '(mapReduce loc fn ifEmpty reduce !right)
    op !fl !fr
```

This function can be either simulated locally (possibly with faults injected for testing purposes), or run atop a distributed pool of compute. 

If you'd like to learn more about the project, [this Strange Loop talk is a good introduction](https://www.youtube.com/watch?v=gCWtkvDQ2ZI). You can also follow along with [project website](https://unisonweb.org) or you can also say hello or lurk [in the Slack chat](https://unisonweb.org/slack).

We are currently alpha testing Unison. If you'd like to participate in alpha testing, you can go to [the docs site](https://www.unisonweb.org/docs) to get started.

Building using Stack
-----

If these instructions don't work for you or are incomplete, please file an issue.

The build uses [Stack](http://docs.haskellstack.org/). If you don't already have it installed, [follow the install instructions](http://docs.haskellstack.org/en/stable/README.html#how-to-install) for your platform.  (Hint: `brew update && brew install stack`)

```sh
$ git clone https://github.com/unisonweb/unison.git
$ cd unison
$ stack --version # we'll want to know this version if you run into trouble
$ stack build && stack exec tests && stack exec unison
```

See [`development.markdown`](development.markdown) for a list of build commands you'll likely use during development.
