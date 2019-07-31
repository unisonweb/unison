let
  shared = import ./nix/shared.nix { };
in
  { inherit (shared)
      easytest
      haskeline
      unison-parser-typechecker
    ;
  }
