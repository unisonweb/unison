{ mkDerivation, base, free, ghcjs-dom, ghcjs-base, mtl, reflex, reflex-dom, 
  semigroups, stdenv, text, transformers, unison-shared }:
mkDerivation {
  pname = "unison-editor";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [ base free ghcjs-base ghcjs-dom mtl reflex reflex-dom 
                   semigroups text transformers unison-shared ];
  homepage = "http://unisonweb.org";
  description = "The Unison programming language and platform";
  license = stdenv.lib.licenses.mit;
}
