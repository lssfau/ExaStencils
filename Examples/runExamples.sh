#!/bin/bash

source examples.sh

echo running executables for $configList
echo 

callPath=$(pwd)

for config in $configList; do
  echo running $config ...
  cd $callPath/generated/${config##*/}
  time ./exastencils
  echo 
done
