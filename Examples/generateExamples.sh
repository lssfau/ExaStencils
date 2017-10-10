#!/bin/bash

platform=lib/linux.platform

source examples.sh

echo generating code for $configList

for config in $configList; do
  echo generating $config
  java -cp ../Compiler/Compiler.jar Main $config.settings $config.knowledge $platform
done
