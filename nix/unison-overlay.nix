final: prev: {
  # a wrapped version of stack that passes the necessary flags to use
  # the nix provided ghc.
  unison-stack = prev.symlinkJoin {
    name = "stack";
    paths = [ final.stack ];
    buildInputs = [ final.makeWrapper ];
    postBuild =
      let
        flags = [ "--no-nix" "--system-ghc" "--no-install-ghc" ];
        add-flags =
          "--add-flags '${prev.lib.concatStringsSep " " flags}'";
      in
      ''
        wrapProgram "$out/bin/stack" ${add-flags}
      '';
  };
}
