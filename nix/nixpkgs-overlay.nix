{ versions }:
final: prev: {
  unison-hls = final.haskell-language-server.override {
    # build with our overridden haskellPackages that have our pinned
    # version of ormolu and hls
    haskellPackages = final.haskell.packages."ghc${versions.ghc}";
    dynamic = true;
    supportedGhcVersions = [ versions.ghc ];
  };
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ghcunison = prev.haskell.packages."ghc${versions.ghc}".extend (hfinal: hprev:
        let inherit (prev.haskell.lib) overrideCabal; in {
          # dependency overrides for ormolu 0.5.2.0
          haskell-language-server =
            let
              p = hfinal.callHackageDirect
                {
                  pkg = "haskell-language-server";
                  ver = versions.hls;
                  sha256 = "0kp586yc162raljyd5arsxm5ndcx5zfw9v94v27bkjg7x0hp1s8b";
                }
                {
                  hls-fourmolu-plugin = null;
                  hls-stylish-haskell-plugin = null;
                  hls-hlint-plugin = null;
                  hls-floskell-plugin = null;
                };
              override = drv: {
                doCheck = false;
                configureFlags = (drv.configureFlags or [ ]) ++ [
                  "-f-fourmolu"
                  "-f-stylishhaskell"
                  "-f-hlint"
                  "-f-floskell"
                ];
              };
            in
            overrideCabal p override;
        });
    };
  };
}
