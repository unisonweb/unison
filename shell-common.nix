nameRaw: plats: let
  name = "unison-${nameRaw}";
  localPkgs = import <nixpkgs> {};
  unisonPkgs = import ./env.nix;
  addCabalInstall = drv: {
    buildDepends = drv.buildDepends ++ [ localPkgs.haskellPackages.cabal-install ];
  };
in with builtins; with unisonPkgs.nixpkgs.stdenv.lib; let
  f = plat: {
    name = plat;
    value = (unisonPkgs.nixpkgs.haskell.lib.overrideCabal
      (getAttr name (getAttr plat unisonPkgs))
      addCabalInstall).env;
  };

in listToAttrs (map f plats)
