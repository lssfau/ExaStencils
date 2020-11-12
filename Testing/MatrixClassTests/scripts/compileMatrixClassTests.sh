#!/bin/bash

platform=lib/linux.platform

source MatrixClassTests.sh


for config in $configList; do
  base="$(basename $config)"
  path="$(dirname $config)"
 echo compiling $base ...
  cd ./$config/output/
  make -j 8  
done
echo done
