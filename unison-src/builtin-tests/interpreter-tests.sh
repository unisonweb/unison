#!/bin/bash
set -ex

ucm=$(stack exec -- which unison)
echo "$ucm"

runtime_tests_version="@unison/runtime-tests/main"
echo $runtime_tests_version

codebase=${XDG_CACHE_HOME:-"$HOME/.cache"}/unisonlanguage/runtime-tests.unison

runtime_tests_version="$runtime_tests_version" \
    envsubst '$runtime_tests_version' \
    < unison-src/builtin-tests/interpreter-tests.tpl.md \
    > unison-src/builtin-tests/interpreter-tests.md
echo "$ucm" transcript.fork -C $codebase -S $codebase unison-src/builtin-tests/interpreter-tests.md
time "$ucm" transcript.fork -C $codebase -S $codebase unison-src/builtin-tests/interpreter-tests.md
