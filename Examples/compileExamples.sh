#!/bin/bash

platform=lib/linux.platform

source examples.sh

echo compiling code for $configList
echo 

callPath=$(pwd)

for config in $configList; do
  echo compiling $config ...
  cd $callPath/generated/${config##*/}
  time make -j 8 > $callPath/Debug/${config##*/}_makeResult.txt
  echo 
done
