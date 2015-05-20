with (import <nixpkgs> {}).pkgs;
(haskellngPackages.callPackage ./. {}).env
