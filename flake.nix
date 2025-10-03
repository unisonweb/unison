{
  description = "Unison";

  nixConfig = {
    allow-import-from-derivation = true;
    extra-substituters = ["https://unison.cachix.org"];
    extra-trusted-public-keys = ["unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="];
    ## This allows derivations with `__noChroot` set to run outside the sandbox.
    sandbox = "relaxed";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    haskellNix.url = "github:input-output-hk/haskell.nix/2025.02.09";
    nixpkgs.follows = "haskellNix/nixpkgs";
    nixpkgs-release.url = "github:NixOS/nixpkgs/release-24.05";
    systems.follows = "flake-utils/systems";
  };

  outputs = {
    flake-utils,
    haskellNix,
    nixpkgs,
    nixpkgs-release,
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
          (import ./nix/dependencies.nix {nixpkgs = nixpkgs-release;})
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
