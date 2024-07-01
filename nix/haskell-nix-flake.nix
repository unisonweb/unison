{
  lib,
  pkgs,
  unison-project,
  versions,
}: let
  haskell-nix-flake = unison-project.flake {};
  commonShellArgs = args:
    args
    // {
      # workaround:
      # https://github.com/input-output-hk/haskell.nix/issues/1793
      # https://github.com/input-output-hk/haskell.nix/issues/1885
      allToolDeps = false;
      additional = hpkgs: with hpkgs; [Cabal stm exceptions ghc ghc-heap];
      buildInputs = let
        native-packages =
          lib.optionals pkgs.stdenv.isDarwin
          (with pkgs.darwin.apple_sdk.frameworks; [Cocoa]);
      in
        (args.buildInputs or [])
        ++ [
          pkgs.cachix
          pkgs.glibcLocales
          pkgs.hpack
          pkgs.pkg-config
          pkgs.stack-wrapped
          pkgs.zlib
        ]
        ++ native-packages;
      # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/11042
      shellHook = ''
        export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
      '';
      tools =
        (args.tools or {})
        // {
          cabal = {};
          ormolu = {version = versions.ormolu;};
          haskell-language-server = {
            version = versions.hls;
            modules = [
              {
                packages.haskell-language-server.components.exes.haskell-language-server.postInstall = ''
                  ln -sr "$out/bin/haskell-language-server" "$out/bin/haskell-language-server-wrapper"
                '';
              }
            ];
            # specify flags via project file rather than a module override
            # https://github.com/input-output-hk/haskell.nix/issues/1509
            cabalProject = ''
              packages: .
              package haskell-language-server
                flags: -brittany -fourmolu -stylishhaskell -hlint
              constraints: ormolu == ${versions.ormolu}
            '';
          };
        };
    };

  shellFor = args: unison-project.shellFor (commonShellArgs args);

  localPackages = lib.filterAttrs (k: v: v.isLocal or false) unison-project.hsPkgs;
in
  haskell-nix-flake
  // {
    checks =
      haskell-nix-flake.checks
      // {
        ## This check has a test that tries to write to $HOME, so we give it a fake one.
        "unison-cli:test:cli-tests" = haskell-nix-flake.checks."unison-cli:test:cli-tests".overrideAttrs (old: {
          ## The builder here doesn’t `runHook preBuild`, so we just prepend onto `buildPhase`.
          buildPhase =
            ''
              export HOME="$TMP/fake-home"
              mkdir -p "$HOME"
            ''
            + old.buildPhase or "";
        });
      };

    defaultPackage = haskell-nix-flake.packages."unison-cli-main:exe:unison";

    devShells = let
      mkDevShell = pkg:
        shellFor {
          packages = _hpkgs: [pkg];
          ## Enabling Hoogle causes us to rebuild GHC.
          withHoogle = false;
        };
    in
      {
        local = shellFor {
          packages = _hpkgs: builtins.attrValues localPackages;
          withHoogle = false;
        };
      }
      // pkgs.lib.mapAttrs (_name: mkDevShell) localPackages;
  }
