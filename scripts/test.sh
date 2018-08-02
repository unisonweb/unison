#!/bin/sh
stack build && stack exec tests && (cd runtime-jvm && sbt benchmark/compile && sbt test)
