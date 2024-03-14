{ pkgs, haskell-nix }:

{
  ucm = pkgs.dockerTools.buildImage {
    name = "ucm";
    tag = "latest";
    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = [ haskell-nix."unison-cli:exe:unison" ];
      pathsToLink = [ "/bin" ];
    };
    config = {
      Cmd = [ "/bin/unison" ];
    };
  };
}
