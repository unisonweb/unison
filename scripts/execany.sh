#!/usr/bin/env bash
source="$1"
unison_files_changed="$source"
haskell_needs_rebuild=x
scala_needs_rebuild=x
haskell_files_changed=
scala_files_changed=
binary=`mktemp`

function build_haskell {
  if [ -n "$haskell_needs_rebuild" ]; then
    echo "Building typechecker..."
    stack build && echo "Typechecker built!" && haskell_needs_rebuild=""
  fi
}

function build_scala {
  if [ -n "$scala_needs_rebuild" ]; then
    echo "Building runtime..."
    (cd runtime-jvm; sbt main/compile) && echo "Runtime built!" && scala_needs_rebuild=""
  fi
}

function typecheck {
  build_haskell && \
  echo "Parsing/typechecking $source" && \
  stack exec bootstrap "$source" "$binary"
}

function execute {

  build_scala && \
  echo "Executing $source ($binary)" && \
  scala \
    -cp runtime-jvm/main/target/scala-2.12/classes org.unisonweb.Bootstrap \
    "$binary"
}

function horizontal_line {
  cols=`tput cols`
  char="="
  printf "%0.s$char" $(seq 1 $cols)
  echo
}

function wait {
  if [ -n "$source" ]; then
    echo "Waiting for changes (selected: $source)..."
  else
    echo "Waiting for changes (no Unison source selected)..."
  fi
}

function go {
  horizontal_line
  typecheck && execute
  wait
}

function process_batch {
  if [ -n "$unison_files_changed" ]; then
    go
  elif [ -n "$source" ]; then
      if [ -n "$haskell_files_changed" ] || [ -n "$scala_files_changed" ]; then
        go
      fi
  elif [ -n "$haskell_files_changed" ]; then
    build_haskell
    if [ -n "$scala_files_changed" ]; then
      build_scala
    fi
    wait
  elif [ -n "$scala_files_changed" ]; then
    build_scala
    wait
  fi
  haskell_files_changed=
  scala_files_changed=
  unison_files_changed=
}

if [ -z "$binary" ]; then
  echo "error running `mktemp` to generate a temporary file name"
  exit 1
fi

build_haskell
if [ -n "$source" ]; then process_batch; else wait; fi

while IFS= read -r changed; do
  # echo "debug: $changed"
  case $changed in
    NoOp)
      process_batch
      ;;
    *.hs)
      echo "detected change in $changed"
      hasktags -cx parser-typechecker
      haskell_needs_rebuild=x
      haskell_files_changed=x
      ;;
    *.scala)
      echo "detected change in $changed"
      scala_needs_rebuild=x
      scala_files_changed=x
      ;;
    *.u|*.uu)
      echo "detected change in $changed"
      source="$changed"
      unison_files_changed=x
      ;;
  esac
done
