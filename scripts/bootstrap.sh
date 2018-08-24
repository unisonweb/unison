#!/usr/bin/env bash
source="$1"
if [ -z "$source" ]; then
  echo "usage: $0 <file.u>"
  exit 1
fi
binary=`mktemp`
stack build && stack exec bootstrap "$source" "$binary" && (cd runtime-jvm; sbt "main/run $binary")
