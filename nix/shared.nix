{ compiler ? "ghc844" }:

let
  overlayShared = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
            overrides =
              let
                dontHaddock = pkgsOld.haskell.lib.dontHaddock;
                failOnAllWarnings = pkgsOld.haskell.lib.failOnAllWarnings;

                extension =
                  haskellPackagesNew: haskellPackagesOld: {
                    easytest =
                      # failOnAllWarnings
                        (haskellPackagesNew.callCabal2nix
                          "easytest"
                          ../yaks/easytest
                          { }
                        );
                    haskeline =
                      failOnAllWarnings
                        (haskellPackagesNew.callCabal2nix
                          "haskeline"
                          ../yaks/haskeline
                          { }
                        );
                    unison-parser-typechecker =
                      dontHaddock
                      # failOnAllWarnings
                        (haskellPackagesNew.callCabal2nix
                          "unison-parser-typechecker"
                          ../parser-typechecker
                          { }
                        );
                  };

              in
                pkgsNew.lib.fold
                  pkgsNew.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  [ (pkgsNew.haskell.lib.packagesFromDirectory { directory = ./.; })

                    extension
                  ];
          }
        );
      };
    };

    unison-parser-typechecker-static = pkgsOld.haskell.lib.justStaticExecutables pkgsNew.haskell.packages."${compiler}".unison-parser-typechecker;

    unison-parser-typechecker-image = pkgsOld.dockerTools.buildImage {
      name = "unison";
      fromImage = pkgsOld.dockerTools.pullImage {
        imageName = "alpine";
        imageDigest = "sha256:6a92cd1fcdc8d8cdec60f33dda4db2cb1fcdcacf3410a8e05b3741f44a9b5998";
        sha256 = "03pnhggr478cp7hap0z9daccq1k1xglna2zm2gw0ynq8qjp6w9dl";
      };
      config.Cmd = [
        "${pkgsNew.unison-parser-typechecker-static}/bin/ucm"
      ];
    };
  };

  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src {
    config = {};
    overlays = [ overlayShared ];
  };

  pkgs-linux = pkgs // {
    system = "x86_64-linux";
  };

in
  rec {
    inherit (pkgs-linux)
      unison-parser-typechecker-static
      unison-parser-typechecker-image
    ;

    inherit (pkgs.haskell.packages."${compiler}")
      easytest
      haskeline
      unison-parser-typechecker
    ;

    shell-easytest = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".easytest).env;
    shell-haskeline = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".haskeline).env;
    shell-unison-parser-typechecker = (pkgs.haskell.lib.doBenchmark pkgs.haskell.packages."${compiler}".unison-parser-typechecker).env;
  }
