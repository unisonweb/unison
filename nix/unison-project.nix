{pkgs}: let
  ## This needs to match the version from the stack.yaml `resolver`. See cdepillabout/stacklock2nix#4. We avoid thinking
  ## about it by having the resolver match the LTS that Nixpkgs’ Haskell package set was built from.
  baseHaskellPkgSet = pkgs.haskellPackages;
in
  pkgs.stacklock2nix {
    inherit baseHaskellPkgSet;
    stackYaml = ../stack.yaml;
    additionalHaskellPkgSetOverrides = hfinal: hprev: {
      unison-cli = hprev.unison-cli.overrideAttrs (old: {
        ## Don’t run unison-cli checks on macOS, because the test-suite requires access to `security` (because of TLS).
        ## NixOS/nixpkgs#297775 is maybe a good entry to the discussions.
        doCheck = !pkgs.stdenv.isDarwin;
      });

      ## Tests fail (or require network access) in some packages.
      ##
      ## NB: If we use nixpkgs-unstable instead of release-YY.MM, then many of these are likely handled upstream,
      ##     because Haskell development on Nix merges (into unstable) often, but backports (into release branches)
      ##     rarely.
      avro = pkgs.haskell.lib.dontCheck hprev.avro;
      base32 = pkgs.haskell.lib.dontCheck hprev.base32;
      conduit-extra = pkgs.haskell.lib.dontCheck hprev.conduit-extra;
      data-clist = pkgs.haskell.lib.dontCheck hprev.data-clist;
      fsnotify = pkgs.haskell.lib.dontCheck hprev.fsnotify;
      doctest-discover = pkgs.haskell.lib.dontCheck hprev.doctest-discover;
      haskeline = pkgs.haskell.lib.dontCheck hprev.haskeline;
      hs-mcp = pkgs.haskell.lib.dontCheck hprev.hs-mcp;
      http2 = pkgs.haskell.lib.dontCheck hprev.http2;
      network = pkgs.haskell.lib.dontCheck hprev.network;
      pvar = pkgs.haskell.lib.dontCheck hprev.pvar;
      servant-client = pkgs.haskell.lib.dontCheck hprev.servant-client;
      streaming-commons = pkgs.haskell.lib.dontCheck hprev.streaming-commons;
      tasty-discover = pkgs.haskell.lib.dontCheck hprev.tasty-discover;

      ## stacklock2nix doesn’t yet support `allow-newer-deps`, so that’s duplicated here. See
      ## cdepillabout/stacklock2nix#54.
      fuzzyfind = pkgs.haskell.lib.doJailbreak hprev.fuzzyfind;
      lock-file = pkgs.haskell.lib.doJailbreak hprev.lock-file;
      numerals = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak hprev.numerals);
    };
    additionalDevShellNativeBuildInputs = stacklockHaskellPkgSet: [
      ## NB: Packages that need to be compiled with the same GHC as our project should come from `baseHaskellPkgSet`,
      ##     not `pkgs`.
      baseHaskellPkgSet.haskell-language-server
      baseHaskellPkgSet.weeder
      pkgs.cabal-install
      pkgs.cachix
      pkgs.gettext # for envsubst, used by unison-src/builtin-tests/interpreter-tests.sh
      pkgs.git
      pkgs.hpack
      pkgs.jq # helpful when pushing to Cachix
      pkgs.ormolu
      pkgs.stack-wrapped
    ];
    devShellArgsModifier = args: args // {withHoogle = true;};
  }
