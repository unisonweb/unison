{ mkDerivation, ansi-terminal, base, bytestring, options, patience
, random, stdenv, template-haskell, text, transformers
}:
mkDerivation {
  pname = "chell";
  version = "0.4.0.2";
  sha256 = "10ingy9qnbmc8cqh4i9pskcw43l0mzk8f3d76b3qz3fig5ary3j9";
  libraryHaskellDepends = [
    ansi-terminal base bytestring options patience random
    template-haskell text transformers
  ];
  description = "A simple and intuitive library for automated testing";
  license = stdenv.lib.licenses.mit;
}
