#!/bin/bash -l

if [ $# -ne 2 ]; then
  echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH>"
fi

HOSTLIST=$(sinfo -h --partition=work -o "%n" | grep rome1) # TODO remove grep
for HOST in ${HOSTLIST}; do
  echo "benchmark-$1-$HOST:"
  echo -e "\tstage: benchmark"
  echo -e "\textends: .benchmark_template"
  echo -e "\tvariables:"
  echo -e "\t\tEXA_PROBLEM_PATH: "$2""
  echo
done
