{
  pkgs,
  haskell-nix,
}: {
  ucm = pkgs.dockerTools.buildLayeredImage {
    name = "ucm";
    tag = "latest";
    contents = with pkgs; [cacert fzf];
    config.Cmd = ["${haskell-nix."unison-cli-main:exe:unison"}/bin/unison"];
  };
}
