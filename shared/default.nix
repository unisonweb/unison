{ mkDerivation, aeson, base, base64-bytestring, bytestring
, containers, mtl, prelude-extras, stdenv, text, transformers, vector
}:
mkDerivation {
  pname = "unison-shared";
  version = "0.1";
  src = ./.;
  buildDepends = [
    aeson base base64-bytestring bytestring containers mtl text
    transformers prelude-extras vector
  ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
}
