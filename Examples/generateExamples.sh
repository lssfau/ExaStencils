#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu" ]]; then
  platform=lib/linux.platform
elif [[ "$OSTYPE" == "darwin"* ]]; then
  platform=lib/mac.platform
fi

source examples.sh

echo generating code for $configList
echo 

for config in $configList; do
  echo generating $config ... 
  time java -cp ../Compiler/Compiler.jar Main $config.settings $config.knowledge $platform > Debug/${config##*/}_generateResult.txt
  echo 
done
