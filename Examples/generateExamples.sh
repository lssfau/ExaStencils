#!/bin/bash

if [[ "$OSTYPE" == "linux-gnu" ]]; then
        platform=lib/linux.platform
elif [[ "$OSTYPE" == "darwin"* ]]; then
	platform=lib/mac.platform
fi

source examples.sh

echo generating code for $configList

for config in $configList; do
  echo generating $config
  java -cp ../Compiler/Compiler.jar Main $config.settings $config.knowledge $platform
done
