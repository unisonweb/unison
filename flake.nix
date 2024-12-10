{
  description = "Unison";

  nixConfig = {
    extra-substituters = ["https://unison.cachix.org"];
    extra-trusted-public-keys = ["unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="];
    ## This allows derivations with `__noChroot` set to run outside the sandbox.
    sandbox = "relaxed";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs";
    systems.follows = "flake-utils/systems";
  };

  outputs = {
    flake-utils,
    haskellNix,
    nixpkgs,
    self,
    systems,
  }:
    flake-utils.lib.eachSystem (import systems)
    (system: let
      versions = import ./nix/versions.nix {inherit (nixpkgs) lib;};
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [
          haskellNix.overlay
          (import ./nix/dependencies.nix)
        ];
      };
      unison-project = import ./nix/unison-project.nix {
        inherit (nixpkgs) lib;
        inherit (pkgs) haskell-nix;
      };
      haskell-nix-flake = import ./nix/haskell-nix-flake.nix {
        inherit pkgs unison-project versions;
        inherit (nixpkgs) lib;
      };
      renameAttrs = fn:
        nixpkgs.lib.mapAttrs' (name: value: {
          inherit value;
          name = fn name;
        });
    in
      ## These are optional, since we set them in ./nix/haskell-nix-flake.nix, but sticking with the default versions
      ## from Nixpkgs can help with caching and make sure the packages stay up-to-date. They can be removed if there is
      ## a reason to use a different version.
      assert pkgs.cabal-install.version == versions.cabal;
      assert pkgs.haskell-language-server.version == versions.hls;
      assert pkgs.ormolu.version == versions.ormolu;
      ## This one is required. Otherwise Nix may build with a different Stack than is exposed in the shell.
      assert pkgs.stack.version == versions.stack; {
        packages =
          renameAttrs (name: "component-${name}") haskell-nix-flake.packages
          // renameAttrs (name: "docker-${name}") (import ./nix/docker.nix {
            inherit pkgs;
            haskell-nix = haskell-nix-flake.packages;
          })
          // {
            default = haskell-nix-flake.defaultPackage;
            all = pkgs.symlinkJoin {
              name = "all";
              paths = let
                all-other-packages =
                  builtins.attrValues (builtins.removeAttrs self.packages."${system}" [
                    "all"
                    "docker-ucm" # this package doesn’t produce a directory
                  ]);
                devshell-inputs =
                  builtins.concatMap
                  (devShell: devShell.buildInputs ++ devShell.nativeBuildInputs)
                  (builtins.attrValues self.devShells."${system}");
              in
                all-other-packages ++ devshell-inputs;
            };
          };

        apps =
          renameAttrs (name: "component-${name}") haskell-nix-flake.apps
          // {default = self.apps."${system}"."component-unison-cli-main:exe:unison";};

        devShells =
          renameAttrs (name: "cabal-${name}") haskell-nix-flake.devShells
          // {default = self.devShells."${system}".cabal-local;};

        checks = renameAttrs (name: "component-${name}") haskell-nix-flake.checks;

        formatter = pkgs.alejandra;
      });
}
