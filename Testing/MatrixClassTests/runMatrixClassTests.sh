#!/bin/bash

source MatrixClassTests.sh

for config in $configList; do
  base="$(basename $config)"
  path="$(dirname $config)"
  echo running $base ...
  cd $config/output/
  ./exastencils
done
echo done
