#!/bin/bash -l

# utility script to generate pipeline stages for CB

if [ $# -ne 5 ]; then
  echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH> <EXASLANG_FILES> <KNOWLEDGE_FILE> <KNOWLEDGE_FILE_CUDA>"
fi

PROBLEM_NAME=$1
PROBLEM_PATH=$2
EXASLANG=$3
KNOWLEDGE=$4
KNOWLEDGE_CUDA=$5

echo -e "bench_gen:$PROBLEM_NAME:"
echo -e "    stage: bench_gen"
echo -e "    extends: .gen_bench_job_template"
echo -e "    variables:"
echo -e "        EXA_PROBLEM_NAME: \"$PROBLEM_NAME\""
echo -e "        EXA_PROBLEM_PATH: \"$PROBLEM_PATH\""
echo -e "        EXASLANG_FILES: \"$EXASLANG\""
echo -e "        KNOWLEDGE_FILE: \"$KNOWLEDGE\""
echo

echo -e "bench_pipe:$PROBLEM_NAME:"
echo -e "    stage: bench_pipe"
echo -e "    trigger:"
echo -e "        include:"
echo -e "            - artifact: Benchmark/$PROBLEM_NAME-pipe.yml"
echo -e "              job: bench_gen:$PROBLEM_NAME"
echo -e "        strategy: depend"
echo -e "    variables:"
echo -e "        EXA_PROBLEM_NAME: \"$PROBLEM_NAME\""
echo -e "        EXA_PROBLEM_PATH: \"$PROBLEM_PATH\""
echo -e "        EXASLANG_FILES: \"$EXASLANG\""
echo -e "        KNOWLEDGE_FILE: \"$KNOWLEDGE\""
echo

echo -e "bench_pipe:${PROBLEM_NAME}_CUDA:"
echo -e "    stage: bench_pipe"
echo -e "    extends: .cuda_job_template"
echo -e "    variables:"
echo -e "        SLURM_NODELIST: \"medusa\""
echo -e "        EXA_PROBLEM_NAME: \"${PROBLEM_NAME}_CUDA\""
echo -e "        EXA_PROBLEM_PATH: \"$PROBLEM_PATH\""
echo -e "        EXASLANG_FILES: \"$EXASLANG\""
echo -e "        KNOWLEDGE_FILE: \"$KNOWLEDGE_CUDA\""
echo -e "        PLATFORM_FILE: \"Platform/medusa_QuadroRTX6000.platform\""
echo