{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "patience";
  version = "0.1.1";
  sha256 = "0qyv20gqy9pb1acy700ahv70lc6vprcwb26cc7fcpcs4scsc7irm";
  revision = "1";
  editedCabalFile = "0xj4hypjnhsn5jhs66l9wwhpkn5pbd8xmx7pgcy2ib08cz1087y7";
  libraryHaskellDepends = [ base containers ];
  description = "Patience diff and longest increasing subsequence";
  license = stdenv.lib.licenses.bsd3;
  hydraPlatforms = stdenv.lib.platforms.none;
}
