#!/bin/bash

source examples.sh

echo running executables for $configList
echo 

callPath=$(pwd)

for config in $configList; do
  echo running $config ...
  printf "\033]2;running $config\007"
  cd $callPath/generated/${config##*/}
  time ./exastencils
  echo 
done
printf "\033]0;\a"
