{
  cacert,
  dockerTools,
  fzf,
  unisonPkgs,
}: dockerTools.buildLayeredImage {
  name = "ucm";
  tag = "latest";
  contents = [cacert fzf];
  config.Cmd = ["${unisonPkgs.unison-cli-main}/bin/unison"];
}
