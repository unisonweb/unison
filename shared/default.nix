{ mkDerivation, aeson, base, base64-bytestring, bifunctors, bytestring
, containers, comonad, free, murmur-hash, mtl, prelude-extras
, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, text, transformers, vector
}:
mkDerivation {
  pname = "unison-shared";
  version = "0.1";
  src = ./.;
  buildDepends = [
    aeson base base64-bytestring bifunctors bytestring containers free 
    murmur-hash mtl text transformers prelude-extras vector
  ];
  testDepends = [
    base containers tasty tasty-hunit tasty-quickcheck tasty-smallcheck
    transformers
  ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
}
