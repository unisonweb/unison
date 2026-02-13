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
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.11-darwin";
    stacklock2nix.url = "github:cdepillabout/stacklock2nix/v5.2.1";
    systems.follows = "flake-utils/systems";
  };

  outputs = {
    flake-utils,
    nixpkgs,
    self,
    systems,
    stacklock2nix,
  }:
    flake-utils.lib.eachSystem (import systems)
    (system: let
      versions = import ./nix/versions.nix {inherit (nixpkgs) lib;};
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        stacklock2nix.overlay
        (import ./nix/dependencies.nix)
      ];
      unisonProject = import ./nix/unison-project.nix {inherit pkgs;};
      unisonPkgs = builtins.listToAttrs (map (pkg: {
          name = pkg.pname;
          value = pkg;
        })
        (unisonProject.localPkgsSelector unisonProject.pkgSet));
    in
      ## These ensure that our version files reflect the versions we actually use in Nix. In future: generate the
      ## version files _from_ Nix.
      assert pkgs.cabal-install.version == versions.cabal;
      assert pkgs.haskell-language-server.version == versions.hls;
      assert pkgs.hpack.version == versions.hpack;
      assert pkgs.ormolu.version == versions.ormolu;
      assert pkgs.stack.version == versions.stack;
      assert pkgs.haskellPackages.weeder.version == versions.weeder; {
        packages =
          unisonPkgs
          // {
            default = unisonPkgs.unison-cli-main;
            docker-ucm = pkgs.callPackage ./nix/docker.nix {inherit unisonPkgs;};
            all = pkgs.symlinkJoin {
              name = "all";
              paths = let
                all-other-packages = builtins.attrValues (builtins.removeAttrs self.packages."${system}" [
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

        devShells.default = unisonProject.devShell;

        formatter = pkgs.alejandra;
      });
}
