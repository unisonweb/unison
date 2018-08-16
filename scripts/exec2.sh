#!/usr/bin/env bash
source="$1"
changed="$2"
if [ -z "$changed" ]; then
  echo "usage: $0 <file.u> <file.(u|hs|scala)>"
  exit 1
fi

binary=`mktemp`

if [ -z "$binary" ]; then
  echo "error running `mktemp` to generate a temporary file name"
  exit 1
fi

function build_haskell {
  stack build
}

function build_scala {
  (cd runtime-jvm; sbt main/compile)
}

function run_unison {
  horizontal_line

  echo "Parsing/typechecking..." &&
    stack exec bootstrap "$source" "$binary" &&
    echo "Executing..." &&
    scala \
      -cp runtime-jvm/main/target/scala-2.12/classes org.unisonweb.Bootstrap \
      "$binary"

  echo "Waiting for changes..."
}

function horizontal_line {
  cols=`tput cols`
  char="="
  printf "%0.s$char" $(seq 1 $cols)
  echo
}

case $changed in
  *.hs) echo "detected change in haskell file: $changed" && build_haskell && run_unison;;
  *.scala) echo "detected change in scala file: $changed" && build_scala && run_unison;;
  *$source) echo "detected change in unison file: $changed" && run_unison;;
  # *) echo "detected change in other file: $changed"
esac
