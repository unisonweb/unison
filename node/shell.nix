with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
  overrides = self: super: {
    unison-shared = self.callPackage ../shared {};
    unison-node = self.callPackage ./. {};
  };
};
in modifiedHaskellPackages.unison-node.env
