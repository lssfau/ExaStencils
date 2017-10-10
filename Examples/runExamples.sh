#!/bin/bash

platform=lib/linux.platform

source examples.sh

echo running executable for $configList

callPath=$(pwd)

for config in $configList; do
  echo running $config
  cd $callPath/generated/${config##*/}
  time ./exastencils
done
