# Building with Cabal

Cabal isn't officially supported, but Unison can be built and installed with it. You'll need the same GHC used by `stack.yaml` to successfully build its dependencies. The provided project file is in `contrib/`, so you'll need to specify its location on the command line.

* To build all projects:

      cabal build --project-file=contrib/cabal.project all

* Tests can be run with e.g.

      cabal test --project-file=contrib/cabal.project all

* The executable can be installed with

      cabal install --project-file=contrib/cabal.project unison

* The install directory can be modified with the option `--installdir=`

* If you want to load the project in haskell-language-server using Cabal instead of Stack you will need to:
  * Symlink `contrib/cabal.project*` to the project root (for example, `ln -s contrib/cabal.project* ./`)
  * Delete or rename the existing `./hie.yaml`. The default behaviour without `hie.yaml` works with Cabal.

Keeping the Cabal configuration in step with Stack's is part of [updating the toolchain](updating-the-toolchain.md#update-cabal-configuration).
