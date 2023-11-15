{ stack, hpack, pkgs, versions }:
let
  haskell-nix-flake = pkgs.unison-project.flake { };
  commonShellArgs = args:
    args // {
      # workaround:
      # https://github.com/input-output-hk/haskell.nix/issues/1793
      # https://github.com/input-output-hk/haskell.nix/issues/1885
      allToolDeps = false;
      additional = hpkgs: with hpkgs; [ Cabal stm exceptions ghc ghc-heap ];
      buildInputs =
        let
          native-packages = pkgs.lib.optionals pkgs.stdenv.isDarwin
            (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa ]);
        in
        (args.buildInputs or [ ]) ++ [ stack hpack pkgs.pkg-config pkgs.zlib pkgs.glibcLocales ] ++ native-packages;
      # workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/11042
      shellHook = ''
        export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
      '';
      tools =
        (args.tools or { }) // {
          cabal = { };
          ormolu = { version = versions.ormolu; };
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

  shellFor = args: pkgs.unison-project.shellFor (commonShellArgs args);

  localPackages = with pkgs.lib;
    filterAttrs (k: v: v.isLocal or false) pkgs.unison-project.hsPkgs;
  localPackageNames = builtins.attrNames localPackages;
  devShells =
    let
      mkDevShell = pkgName:
        shellFor {
          packages = hpkgs: [ hpkgs."${pkgName}" ];
          withHoogle = true;
        };
      localPackageDevShells =
        pkgs.lib.genAttrs localPackageNames mkDevShell;
    in
    {
      only-tools = shellFor {
        packages = _: [ ];
        withHoogle = false;
      };
      local = shellFor {
        packages = hpkgs: (map (p: hpkgs."${p}") localPackageNames);
        withHoogle = true;
      };
    } // localPackageDevShells;
in
haskell-nix-flake // {
  defaultPackage = haskell-nix-flake.packages."unison-cli:exe:unison";
  inherit (pkgs) unison-project;
  inherit devShells localPackageNames;
  packages = haskell-nix-flake.packages;
}
