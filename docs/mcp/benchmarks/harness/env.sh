# Sourced by the harness scripts. Everything lives under BENCH_ROOT, which is outside /tmp so that it survives a reboot.
export BENCH_ROOT=${BENCH_ROOT:-$HOME/.cache/unison-mcp-bench}
export BIN=${BIN:-$BENCH_ROOT/bin/unison}                  # the frozen executable under test (copy it there before a round)
export BENCHMARK_CODEBASES_DIR=$BENCH_ROOT/codebases        # cache of base codebases for prepare.sh
export HARNESS=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
export BENCHMARKS=$(cd "$HARNESS/.." && pwd)
export BENCH=$BENCH_ROOT/bench                              # one directory per run
export UCM_MERGETOOL=
mkdir -p "$BENCH" "$BENCHMARK_CODEBASES_DIR"
