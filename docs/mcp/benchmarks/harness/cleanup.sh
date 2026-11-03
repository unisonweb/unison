#!/usr/bin/env bash
# usage: cleanup.sh <run name>: stops every server of the run (the worker's and the judges')
source "$(dirname "$0")/env.sh"
for p in "$BENCH/$1"/pid "$BENCH/$1"-j-*/pid; do [ -f "$p" ] && kill "$(cat "$p")" 2>/dev/null; done; true
