#!/bin/bash

mkdir -p debug
time java -cp ../generator/compiler.jar Main ./swe_o0_rk2_l3_supercritical_b1_f28.settings ./swe_o0_rk2_l3_supercritical_b1_f28.knowledge ../generator/linux.platform > ./debug/generate_output.txt
cd generated/swe_o0_rk2_l3_supercritical_b1_f28
time make -j 8 > ../../debug/build_output.txt
time ./exastencils
cd ../..
