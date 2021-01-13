#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu" ]]; then
  platform=lib/linux.platform
elif [[ "$OSTYPE" == "darwin"* ]]; then
  platform=lib/mac.platform
fi
#platform=lib/windows.platform

source examples.sh

echo generating code for $configList
echo

mkdir Debug 2>/dev/null

for config in $configList; do
  echo generating $config ...
  printf "\033]2;generating $config\007"
  TIME=$( time java -cp ../Compiler/lib:../Compiler/Compiler.jar Main $config.settings $config.knowledge $platform > ./Debug/${config##*/}_generateResult.txt; exit ${PIPESTATUS[0]}
)
  RET=$?
  echo $TIME
  if [[ "$RET" -eq "0" ]]; then
    printf "\033[32m\033[1mSuccess\033[0m"
  else
    printf "\033[31m\033[1mFailure\033[0m"
  fi
  echo
done
printf "\033]0;\a"
