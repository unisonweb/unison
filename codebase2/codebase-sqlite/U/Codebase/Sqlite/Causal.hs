module U.Codebase.Sqlite.Causal where


data Causal hc he = RawCausal {
  valueHash :: he,
  parentHashes :: [hc]
}