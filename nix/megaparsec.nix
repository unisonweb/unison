{ mkDerivation, base, bytestring, case-insensitive, containers
, criterion, deepseq, hspec, hspec-discover, hspec-expectations
, mtl, parser-combinators, QuickCheck, scientific, stdenv, text
, transformers, weigh
}:
mkDerivation {
  pname = "megaparsec";
  version = "6.5.0";
  sha256 = "12iggy7qpf8x93jm64zf0g215xwy779bqyfyjk2bhmxqqr1yzgdy";
  revision = "4";
  editedCabalFile = "0ij3asi5vwlhbgwsy6nhli9a0qb7926mg809fsgyl1rnhs9fvpx1";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq mtl
    parser-combinators scientific text transformers
  ];
  testHaskellDepends = [
    base bytestring containers hspec hspec-expectations mtl QuickCheck
    scientific text transformers
  ];
  testToolDepends = [ hspec-discover ];
  benchmarkHaskellDepends = [ base criterion deepseq text weigh ];
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd2;
  hydraPlatforms = stdenv.lib.platforms.none;
}
