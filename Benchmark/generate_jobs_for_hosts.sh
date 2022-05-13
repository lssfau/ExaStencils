#!/bin/bash -l

if [ $# -ne 2 ]
  then
    echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH>"
fi

# Get list of hosts in the testcluster (requires NO_SLURM_SUBMIT=1)
HOSTLIST=$(sinfo -h --partition=work -o "%n" | grep rome1 ) # TODO remove grep
for HOST in ${HOSTLIST}; do
  cat << EOF
benchmark-$1-$HOST:
  stage: benchmark
  extends: .benchmark_template
  variables:
    EXA_PROBLEM_PATH: "$2"
  EOF
done