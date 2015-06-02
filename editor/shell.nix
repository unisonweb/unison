with (import <nixpkgs> {}).pkgs;
let modifiedHaskellPackages = haskellngPackages.override {
  overrides = self: super: {
    unison-shared = self.callPackage ../shared {};
    unison-editor = self.callPackage ./. {};
  };
};
in modifiedHaskellPackages.unison-editor.env
