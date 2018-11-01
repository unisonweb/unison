#!/bin/sh
stack build && stack exec tests && \
  (cd runtime-jvm && \
    sbt bloopInstall && \
    bloop compile benchmark && \
    bloop run main-test)
