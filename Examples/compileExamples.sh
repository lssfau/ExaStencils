#!/bin/bash

platform=lib/linux.platform

source examples.sh

echo compiling code for $configList
echo 

callPath=$(pwd)

for config in $configList; do
  echo compiling $config ...
  printf "\033]2;compiling $config\007"
  cd $callPath/generated/${config##*/}
  TIME=$( time make -j 8 > $callPath/Debug/${config##*/}_makeResult.txt; exit ${PIPESTATUS[0]} )
  echo $TIME
  if [[ "$?" -eq "0" ]]; then
    printf "\033[32m\033[1mSuccess\033[0m"
  else
    printf "\033[31m\033[1mFailure\033[0m"
  fi
  echo 
done
printf "\033]0;\a"
