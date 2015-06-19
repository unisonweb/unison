nameRaw: plats: let
  name = "unison-${nameRaw}";
  nixpkgs = import ./env.nix;
in with builtins; with nixpkgs.stdenv.lib; let
  f = plat: {
    name = plat;
    value = (getAttr name (getAttr plat nixpkgs.unisonPackages)).env;
  };

in listToAttrs (map f plats)
