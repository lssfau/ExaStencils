#!/bin/bash

source examples.sh

echo running executables for $configList
echo 

callPath=$(pwd)

for config in $configList; do
  echo running $config ...
   echo -e '\033]2;'running $config'\007'
  cd $callPath/generated/${config##*/}
  time ./exastencils
  echo 
done
