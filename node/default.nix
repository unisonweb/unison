{ mkDerivation, aeson, applicative-extras, attoparsec, base
, blaze-html, bytes, bytestring, cereal, containers, cryptohash
, directory, filepath, http-types, io-streams, mtl, network, prelude-extras, scotty
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
    http-types io-streams mtl network prelude-extras scotty text transformers
    transformers-compat unison-shared vector
  ];
  testDepends = [
    base tasty tasty-hunit tasty-quickcheck tasty-smallcheck
    unison-shared
  ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
  doHaddock = false;
}
