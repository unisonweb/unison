{
  haskell-nix,
  lib,
}: let
  cleanSource = src:
    lib.cleanSourceWith {
      inherit src;
      filter = path: type: let
        bn = baseNameOf path;
        isHiddenFile = lib.hasPrefix "." bn;
        isFlakeLock = bn == "flake.lock";
        isNix = lib.hasSuffix ".nix" bn;
      in
        !isHiddenFile && !isFlakeLock && !isNix;
    };
in
  haskell-nix.project' {
    src = cleanSource ./..;
    projectFileName = "stack.yaml";
    modules = [
      # enable profiling
      {
        enableLibraryProfiling = true;
        profilingDetail = "none";
      }
    ];
    branchMap = {
      "https://github.com/unisonweb/configurator.git"."e47e9e9fe1f576f8c835183b9def52d73c01327a" = "unison";
      "https://github.com/unisonweb/shellmet.git"."2fd348592c8f51bb4c0ca6ba4bc8e38668913746" = "topic/avoid-callCommand";
    };
  }
