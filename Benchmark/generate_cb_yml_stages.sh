#!/bin/bash -l

# utility script to generate pipeline stages for CB

## generator functions

function generate_pipeline_stages_for_app() {
  if [ $# -ne 5 ]; then
    echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH> <EXASLANG_FILES> <KNOWLEDGE_FILE> <KNOWLEDGE_FILE_CUDA>"
  fi

  PROBLEM_NAME=$1
  PROBLEM_PATH=$2
  EXASLANG=$3
  KNOWLEDGE=$4
  KNOWLEDGE_CUDA=$5

  echo -e "    - bench_gen_$PROBLEM_NAME"
  echo -e "    - bench_pipe_$PROBLEM_NAME"
  echo -e "    - bench_pipe_${PROBLEM_NAME}_CUDA"
}

function generate_pipeline_jobs_for_app() {
  if [ $# -ne 5 ]; then
    echo "Wrong number of arguments. Should be ./<script_name>.sh <EXA_PROBLEM_NAME> <EXA_PROBLEM_PATH> <EXASLANG_FILES> <KNOWLEDGE_FILE> <KNOWLEDGE_FILE_CUDA>"
  fi

  PROBLEM_NAME=$1
  PROBLEM_PATH=$2
  EXASLANG=$3
  KNOWLEDGE=$4
  KNOWLEDGE_CUDA=$5

  echo -e "bench_gen:$PROBLEM_NAME:"
  echo -e "    stage: bench_gen_$PROBLEM_NAME"
  echo -e "    extends: .gen_bench_job_template"
  echo -e "    variables:"
  echo -e "        EXA_PROBLEM_NAME: \"$PROBLEM_NAME\""
  echo -e "        EXA_PROBLEM_PATH: \"$PROBLEM_PATH\""
  echo -e "        EXASLANG_FILES: \"$EXASLANG\""
  echo -e "        KNOWLEDGE_FILE: \"$KNOWLEDGE\""
  echo

  echo -e "bench_pipe:$PROBLEM_NAME:"
  echo -e "    stage: bench_pipe_$PROBLEM_NAME"
  echo -e "    trigger:"
  echo -e "        include:"
  echo -e "            - artifact: Benchmark/$PROBLEM_NAME-child-pipe.yml"
  echo -e "              job: bench_gen:$PROBLEM_NAME"
  echo -e "        strategy: depend"
  echo -e "    variables:"
  echo -e "        EXA_PROBLEM_NAME: \"$PROBLEM_NAME\""
  echo -e "        EXA_PROBLEM_PATH: \"$PROBLEM_PATH\""
  echo -e "        EXASLANG_FILES: \"$EXASLANG\""
  echo -e "        KNOWLEDGE_FILE: \"$KNOWLEDGE\""
  echo

  echo -e "bench_pipe:${PROBLEM_NAME}_CUDA:"
  echo -e "    stage: bench_pipe_${PROBLEM_NAME}_CUDA"
  echo -e "    extends: .cuda_job_template"
  echo -e "    variables:"
  echo -e "        SLURM_NODELIST: \"medusa\""
  echo -e "        EXA_PROBLEM_NAME: \"${PROBLEM_NAME}_CUDA\""
  echo -e "        EXA_PROBLEM_PATH: \"$PROBLEM_PATH\""
  echo -e "        EXASLANG_FILES: \"$EXASLANG\""
  echo -e "        KNOWLEDGE_FILE: \"$KNOWLEDGE_CUDA\""
  echo -e "        PLATFORM_FILE: \"Platform/medusa_QuadroRTX6000.platform\""
  echo
}

## generate yml file

echo -e "variables:"
echo -e "    GIT_STRATEGY: fetch"
echo -e "    GIT_SUBMODULE_STRATEGY: recursive"
echo

echo -e "include:"
echo -e "    - 'Benchmark/.benchmark_templates.yml'"
echo -e "    - 'Benchmark/lock.yml'"
echo

### generator func args

five_point_args=("5pt_Jac_2D" "FivePointStencil" "5pt_Jac_Cell.exa4" "5pt_Jac_Cell.knowledge" "5pt_Jac_Cell_CUDA.knowledge")
opt_flow_args=("2D_FD_OptFlow" "OptFlow2D" "2D_FD_OptFlow.exa4" "2D_FD_OptFlow.knowledge" "2D_FD_OptFlow_CUDA.knowledge")
poisson3D_args=("3D_FD_Poisson" "Poisson3D" "3D_FD_Poisson_fromL4.exa4" "3D_FD_Poisson_fromL4.knowledge" "3D_FD_Poisson_fromL4_CUDA.knowledge")


### generate stage names

echo -e "stages:"
generate_pipeline_stages_for_app "${five_point_args[@]}"
generate_pipeline_stages_for_app "${opt_flow_args[@]}"
generate_pipeline_stages_for_app "${poisson3D_args[@]}"
echo

### generate job names

generate_pipeline_jobs_for_app "${five_point_args[@]}"
generate_pipeline_jobs_for_app "${opt_flow_args[@]}"
generate_pipeline_jobs_for_app "${poisson3D_args[@]}"
