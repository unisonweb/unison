{ mkDerivation, base, directory, fetchgit, filepath, stdenv }:
mkDerivation {
  pname = "fsutils";
  version = "0.1.3";
  src = fetchgit {
    url = "https://github.com/tscholak/fsutils.git";
    sha256 = "1206hndi3ybis269d4p3di0ws9gcn3ck2yj2ywbn7w38jwzgq7w7";
    rev = "e6ed17cb4ebe624c0477f0dbd5b1711f847d136a";
  };
  libraryHaskellDepends = [ base directory filepath ];
  description = "File system utilities for Haskell that are missing from built in libraries";
  license = stdenv.lib.licenses.mit;
  hydraPlatforms = stdenv.lib.platforms.none;
  broken = false;
}
