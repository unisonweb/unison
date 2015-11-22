{ mkDerivation, stdenv,
  base,
  aeson,
  base64-bytestring,
  bifunctors,
  bytestring,
  comonad,
  containers,
  data-default,
  free,
  ghcjs-base,
  ghcjs-dom,
  mtl,
  murmur-hash,
  prelude-extras,
  reflex,
  reflex-dom,
  semigroups,
  text,
  these,
  transformers,
  vector
}:
mkDerivation {
  pname = "unison-editor";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ 
    base
    aeson
    base64-bytestring
    bifunctors
    bytestring
    comonad
    containers
    data-default
    free
    ghcjs-base
    ghcjs-dom
    mtl
    murmur-hash
    prelude-extras
    reflex
    reflex-dom
    semigroups
    text
    these
    transformers
    vector
  ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
}
