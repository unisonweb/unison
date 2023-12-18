final: prev: {
  unison-project = with prev.lib.strings;
    let
      cleanSource = pth:
        let
          src' = prev.lib.cleanSourceWith {
            filter = filt;
            src = pth;
          };
          filt = path: type:
            let
              bn = baseNameOf path;
              isHiddenFile = hasPrefix "." bn;
              isFlakeLock = bn == "flake.lock";
              isNix = hasSuffix ".nix" bn;
            in
            !isHiddenFile && !isFlakeLock && !isNix;
        in
        src';
    in
    final.haskell-nix.project' {
      src = cleanSource ./..;
      projectFileName = "stack.yaml";
      modules = [
        # enable profiling
        {
          enableLibraryProfiling = true;
          profilingDetail = "none";
        }
        # remove buggy build tool dependencies
        ({ lib, ... }: {
          # this component has the build tool
          # `unison-cli:unison` and somehow haskell.nix
          # decides to add some file sharing package
          # `unison` as a build-tool dependency.
          packages.unison-cli.components.exes.cli-integration-tests.build-tools =
            lib.mkForce [ ];
        })
      ];
      branchMap = {
        "https://github.com/unisonweb/configurator.git"."e47e9e9fe1f576f8c835183b9def52d73c01327a" =
          "unison";
        "https://github.com/unisonweb/shellmet.git"."2fd348592c8f51bb4c0ca6ba4bc8e38668913746" =
          "topic/avoid-callCommand";
      };
    };
}
