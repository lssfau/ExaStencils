#!/bin/bash -l

if [ $# -ne 2 ]; then
  echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH>"
fi

# get idle hosts (and filter out drained hosts)
HOST_LIST=$(echo "$(sinfo -t idle -h --partition=work -o "%n"; sinfo -t drain -h -o "%n")" | sort | uniq -u)
HOST_FILTER='aurora1 warmup applem1studio'

# include template
echo "$(<.benchmark_templates.yml)"

# print out stages
echo "stages:"
for HOST in ${HOST_LIST[@]}; do
  if [[ " $HOST_FILTER " =~ .*\ $HOST\ .* ]]; then
    continue
  fi
  echo "    - bench_pipe_$1_$HOST"
done

# generate pipeline
for HOST in ${HOST_LIST[@]}; do
  if [[ " $HOST_FILTER " =~ .*\ $HOST\ .* ]]; then
    continue
  fi

  echo ""
  echo "benchmark-$1-$HOST:"
  echo -e "    stage: bench_pipe_$1_$HOST"
  echo -e "    extends: .bench_pipe_template"
  echo -e "    variables:"
  echo -e "        EXA_PROBLEM_PATH: "$2""
  echo -e "        SLURM_NODELIST: "$HOST""
  echo -e "    needs:"
  echo -e "        - pipeline: \$PARENT_PIPELINE_ID"
  echo -e "          job: bench_gen:$1"
  echo
done
