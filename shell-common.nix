nameRaw: plats: let
  name = "unison-${nameRaw}";
  nixpkgs = import ./env.nix;
in with builtins; with nixpkgs.stdenv.lib; let
  addCabalInstall = drv: {
    buildDepends = drv.buildDepends ++ [ nixpkgs.unisonPackages.ghc7101.cabal-install ];
  };
  f = plat: {
    name = plat;
    value = (nixpkgs.haskell.lib.overrideCabal
      (getAttr name (getAttr plat nixpkgs.unisonPackages))
      addCabalInstall).env;
  };

in listToAttrs (map f plats)
