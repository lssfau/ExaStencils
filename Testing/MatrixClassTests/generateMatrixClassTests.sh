#!/bin/bash
if [[ "$OSTYPE" == "linux-gnu" ]]; then
  platform=linux.platform
elif [[ "$OSTYPE" == "darwin"* ]]; then
  platform=mac.platform
fi
#platform=lib/windows.platform

source matrixClassTests.sh

mkdir Debug 2>/dev/null

for config in $configList; do
  base="$(basename $config)"
  path="$(dirname $config)"
  echo generating Test $base ...
  java -cp ../../Compiler/lib:../../Compiler/Compiler.jar Main $config/$base.settings $config/$base.knowledge $platform 
  echo
  echo
done
echo done
