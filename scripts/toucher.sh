#! /bin/bash

function touch_the_things {
  for x in $(seq 10 1)
  do
    echo $x
    sleep 1
  done
  for i in $(find unison-src/tests -name *.u)
  do
    touch $i
  done
}

touch_the_things &
stack exec unison 

