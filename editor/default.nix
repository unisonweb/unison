{ mkDerivation, reflex, reflex-dom, stdenv, unison-shared }:
mkDerivation {
  pname = "unison-editor";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ reflex reflex-dom unison-shared ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
}
