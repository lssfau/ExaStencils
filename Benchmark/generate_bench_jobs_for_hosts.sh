#!/bin/bash -l

if [ $# -ne 2 ]; then
  echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH>"
fi

HOSTLIST=$(sinfo -h --partition=work -o "%n")
for HOST in ${HOSTLIST}; do
  echo "stages:"
  echo "    - benchmark"
  echo ""
  echo "$(<.benchmark_templates.yml)"
  echo ""
  echo "benchmark-$1-$HOST:"
  echo -e "    stage: benchmark"
  echo -e "    extends: .benchmark_template"
  echo -e "    variables:"
  echo -e "        EXA_PROBLEM_PATH: "$2""
  echo
done
