{ mkDerivation, base, bytestring, HUnit, stdenv, text, uuid
, uuid-types }:
mkDerivation {
  pname = "guid";
  version = "0.1.0";
  sha256 = "b9dfaeffaaeb9e00f63a3af0e72e518231a93158c7e0ea0416cf62888af84eca";
  prePatch = ''
    sed -i '1d' Setup.hs
  '';
  libraryHaskellDepends = [ base bytestring text uuid uuid-types ];
  testHaskellDepends = [ base HUnit ];
  description = "A simple wrapper around uuid";
  license = stdenv.lib.licenses.mit;
  doCheck = false;
}
