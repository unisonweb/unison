*Vault* is a tiny library that provides a single data structure called *vault*.

A *vault* is a type-safe, persistent storage for values of arbitrary types. Like `IORef`, I want to be able to store values of any type in it, but unlike `IORef`, I want the storage space to behave like a persistent, first-class data structure, as appropriate for a purely functional language.

It is analogous to a bank vault, where you can access different bank boxes with different keys; hence the name.

In other words, a vault is an abstract data type with the following basic signature

    data Key a
    data Vault

    newKey :: IO (Key a)
    empty  :: Vault
    lookup :: Key a -> Vault -> Maybe a
    insert :: Key a -> a -> Vault -> Vault
    delete :: Key a -> Vault -> Vault

A few common functions for finite maps, like `adjust` and `union`, are provided as well.


This library was created thanks to the feedback on my blog post [Vault - a persistent store for values of arbitrary types][1].

  [1]: http://apfelmus.nfshost.com/blog/2011/09/04-vault.html


Installation
============
The whole thing is [available on hackage][hackage], so you just have to type

    cabal update
    cabal install vault

  [hackage]: http://hackage.haskell.org/package/vault

Feedback
========
Use the [issue tracker][2] or send an [email to the maintainer][3].

  [2]: https://github.com/HeinrichApfelmus/vault/issues
  [3]: mailto:apfelmus@quantentunnel.de




