let
  shared = import ./nix/shared.nix { };
in
  { inherit (shared)
      unison-parser-typechecker-static
      unison-parser-typechecker-image
    ;
  }
