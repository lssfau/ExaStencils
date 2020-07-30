#!/bin/bash

mkdir -p debug
time java -cp ../generator/compiler.jar Main ./swe_o1_rk2_l4_noPloy_uniform.settings ./swe_o1_rk2_l4_noPloy_uniform.knowledge ../generator/linux.platform > ./debug/generate_output.txt
cd generated/swe_o1_rk2_l4_noPloy_uniform
time make -j 8 > ../../debug/build_output.txt
time ./exastencils
cd ../..
