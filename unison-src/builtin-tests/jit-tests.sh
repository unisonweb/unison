#!/bin/bash
set -ex

ucm=$(stack exec -- which unison)

runtime_tests_version="@unison/runtime-tests/main"
echo $runtime_tests_version

codebase=${XDG_CACHE_HOME:-"$HOME/.cache"}/unisonlanguage/runtime-tests.unison

dir=${XDG_DATA_HOME:-"$HOME/.local/share"}/unisonlanguage/scheme-libs
echo $dir

mkdir -p $dir
cp -r scheme-libs/* $dir/

runtime_tests_version="$runtime_tests_version" \
    envsubst '$runtime_tests_version' \
    < unison-src/builtin-tests/jit-tests.tpl.md \
    > unison-src/builtin-tests/jit-tests.md

# this ought to have the --runtime-path flag passed appropriately
time "$ucm" transcript.fork -C $codebase -S $codebase unison-src/builtin-tests/jit-tests.md
