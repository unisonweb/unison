{ mkDerivation, aeson, applicative-extras, attoparsec, base
, blaze-html, bytes, bytestring, cereal, containers, cryptohash
, directory, filepath, http-types, mtl, prelude-extras, scotty
, stdenv, tasty, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, text, transformers, transformers-compat, unison-shared, vector
}:
mkDerivation {
  pname = "unison-node";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson applicative-extras attoparsec base blaze-html bytes
    bytestring cereal containers cryptohash directory filepath
    http-types mtl prelude-extras scotty text transformers
    transformers-compat unison-shared vector
  ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
  doHaddock = false;
}
