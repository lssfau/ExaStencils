#!/bin/bash -l

if [ $# -ne 2 ]; then
  echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH>"
fi

HOSTLIST=$(sinfo -t idle -h --partition=work -o "%n")
for HOST in ${HOSTLIST}; do
  if [[ "$HOST" != "aurora1" && "$HOST" != "warmup" ]]; then
    echo "stages:"
    echo "    - bench_pipe"
    echo ""
    echo "$(<.benchmark_templates.yml)"
    echo ""
    echo "benchmark-$1-$HOST:"
    echo -e "    stage: bench_pipe"
    echo -e "    extends: .bench_pipe_template"
    echo -e "    variables:"
    echo -e "        EXA_PROBLEM_PATH: "$2""
    echo -e "        SLURM_NODELIST: "$HOST""
    echo -e "        SLURM_CPU_FREQ: high"
    echo -e "    needs:"
    echo -e "        - pipeline: \"$PARENT_PIPELINE_ID\""
    echo -e "          job: bench_gen:$1"
    echo
  fi
done
