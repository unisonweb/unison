{
  description = "Unison";

  nixConfig = {
    extra-substituters = ["https://unison.cachix.org"];
    extra-trusted-public-keys = ["unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
    nixpkgs-unstable,
  }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
    (system: let
      versions = {
        ghc = "928";
        ormolu = "0.5.2.0";
        hls = "2.4.0.0";
        stack = "2.13.1";
        hpack = "0.35.2";
      };
      overlays = [
        haskellNix.overlay
        (import ./nix/haskell-nix-overlay.nix)
        (import ./nix/unison-overlay.nix)
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      haskell-nix-flake = import ./nix/haskell-nix-flake.nix {
        inherit pkgs versions;
        inherit (nixpkgs-packages) stack hpack;
      };
      unstable = import nixpkgs-unstable {
        inherit system;
        overlays = [
          (import ./nix/unison-overlay.nix)
          (import ./nix/nixpkgs-overlay.nix {inherit versions;})
        ];
      };
      nixpkgs-packages = let
        hpkgs = unstable.haskell.packages.ghcunison;
        exe = unstable.haskell.lib.justStaticExecutables;
      in {
        ghc = unstable.haskell.compiler."ghc${versions.ghc}";
        ormolu = exe hpkgs.ormolu;
        hls = unstable.unison-hls;
        stack = unstable.unison-stack;
        unwrapped-stack = unstable.stack;
        hpack = unstable.hpack;
      };
      nixpkgs-devShells = {
        only-tools-nixpkgs = unstable.mkShell {
          name = "only-tools-nixpkgs";
          buildInputs = let
            build-tools = with nixpkgs-packages; [
              ghc
              ormolu
              hls
              stack
              hpack
            ];
            native-packages =
              pkgs.lib.optionals pkgs.stdenv.isDarwin
              (with unstable.darwin.apple_sdk.frameworks; [Cocoa]);
            c-deps = with unstable; [pkg-config zlib glibcLocales];
          in
            build-tools ++ c-deps ++ native-packages;
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.zlib}/lib:$LD_LIBRARY_PATH
          '';
        };
      };

      renameAttrs = fn:
        nixpkgs.lib.mapAttrs' (name: value: {
          inherit value;
          name = fn name;
        });
    in
      assert nixpkgs-packages.ormolu.version == versions.ormolu;
      assert nixpkgs-packages.hls.version == versions.hls;
      assert nixpkgs-packages.unwrapped-stack.version == versions.stack;
      assert nixpkgs-packages.hpack.version == versions.hpack; {
        packages =
          nixpkgs-packages
          // renameAttrs (name: "component-${name}") haskell-nix-flake.packages
          // renameAttrs (name: "docker-${name}") (import ./nix/docker.nix {
            inherit pkgs;
            haskell-nix = haskell-nix-flake.packages;
          })
          // {
            default = haskell-nix-flake.defaultPackage;
            build-tools = pkgs.symlinkJoin {
              name = "build-tools";
              paths = self.devShells."${system}".only-tools-nixpkgs.buildInputs;
            };
            all = pkgs.symlinkJoin {
              name = "all";
              paths = let
                all-other-packages = builtins.attrValues (builtins.removeAttrs self.packages."${system}" ["all" "build-tools"]);
                devshell-inputs =
                  builtins.concatMap
                  (devShell: devShell.buildInputs ++ devShell.nativeBuildInputs)
                  [
                    self.devShells."${system}".only-tools-nixpkgs
                  ];
              in
                all-other-packages ++ devshell-inputs;
            };
          };

        apps =
          renameAttrs (name: "component-${name}") haskell-nix-flake.apps
          // {default = self.apps."${system}"."component-unison-cli-main:exe:unison";};

        devShells =
          nixpkgs-devShells
          // renameAttrs (name: "cabal-${name}") haskell-nix-flake.devShells
          // {default = self.devShells."${system}".only-tools-nixpkgs;};

        checks = renameAttrs (name: "component-${name}") haskell-nix-flake.checks;

        formatter = pkgs.alejandra;
      });
}
