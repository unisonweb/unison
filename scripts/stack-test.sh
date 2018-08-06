#!/bin/sh
stack build && stack exec tests && \
  echo "The stack tests passed. The sbt tests are postponed until git push."
