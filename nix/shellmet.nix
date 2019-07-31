{ mkDerivation, base, markdown-unlit, process, stdenv, text }:
mkDerivation {
  pname = "shellmet";
  version = "0.0.1";
  sha256 = "11c53h3dvhmnkjhcjw1xjr1kx6pvdmayf86i5b6zhpl4q3q2ixlk";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base process text ];
  executableHaskellDepends = [ base text ];
  executableToolDepends = [ markdown-unlit ];
  testHaskellDepends = [ base ];
  description = "Out of the shell solution for scripting in Haskell";
  license = stdenv.lib.licenses.mpl20;
}
