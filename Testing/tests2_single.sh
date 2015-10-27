#!/bin/bash
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=multithread
#SBATCH --nice=100
#SBATCH --time=17
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


TESTING_DIR=${1}
COMPILER=${2}
MAIN=${3}
TEST_DIR=${4}
BIN=${5}
KNOWLEDGE=${6}
L4FILE=${7}
ERROR_MARKER=${8}
LOG_ALL=${9}
LINK=${10}
PROGRESS=${11}


echo "<html><head><meta charset=\"utf-8\"></head><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</pre></body></html>" > "${PROGRESS}"

echo "Generate and compile on machine ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."
echo ""
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

mkdir -p "${TEST_DIR}"
SETTINGS="${TEST_DIR}/settings.txt"


RESULT=$(mktemp --tmpdir=/run/shm || mktemp --tmpdir=/tmp) || {
    echo "ERROR: Failed to create temporary file."
    touch ${ERROR_MARKER}
    echo "${LINK}" >> "${LOG_ALL}"
    exit 0
  }

function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  echo "${LINK}  (maybe requeued)" >> "${LOG_ALL}"
  exit 1
}
trap killed SIGTERM

STARTTIME=$(date +%s)

function cleanup {
  rm "${RESULT}"
  echo "  Removed  ${RESULT}"
  ENDTIME=$(date +%s)
  echo "Runtime: $((${ENDTIME} - ${STARTTIME})) seconds  (target code generation and compilation)"
  echo ""
  echo ""
  echo "-----------------------------------------------------------------------------------------------"
}
trap cleanup EXIT


# build settings file
rm -f "${SETTINGS}"
touch "${SETTINGS}"
echo "outputPath = \"${TEST_DIR}\"" >> "${SETTINGS}"
echo "l4file = \"${L4FILE}\"" >> "${SETTINGS}"
echo "binary = \"${BIN}\"" >> "${SETTINGS}"

echo "Run generator:"
echo "  Created  ${RESULT}: run generator and save its stdout and stderr."
cd ${TESTING_DIR}  # there is no possibility to explicitly set the working directory of the jvm... (changing property user.dir does not work in all situations)
set -o pipefail
srun java -cp "${COMPILER}" ${MAIN} "${SETTINGS}" "${KNOWLEDGE}" 2>&1 | tee "${RESULT}"
RETCODE=$?
    if grep -q "Bad file descriptor" ${RESULT}; then
      echo "restart generation..."
      cleanup # call cleanup directly; no exit trap when requeue is performed (slurm kills this script completly)
      scontrol requeue ${SLURM_JOB_ID}
      sleep 60 # ensure this execution never enters a finished state (for dependences), since scontrol might need some time
    fi
    if [[ ${RETCODE} -ne 0 ]]; then
      echo ""
      echo "ERROR: generator return code unequal to 0."
      echo ""
      touch ${ERROR_MARKER}
      echo "${LINK}" >> "${LOG_ALL}"
      exit 1
    fi
echo ""
echo ""
echo "-----------------------------------------------------------------------------------------------"
echo "Call make:"
srun make -C "${TEST_DIR}" -j ${SLURM_CPUS_ON_NODE}
    if [[ $? -ne 0 ]]; then
      echo ""
      echo "ERROR: make return code unequal to 0."
      echo ""
      touch ${ERROR_MARKER}
      echo "${LINK}" >> "${LOG_ALL}"
      exit 1
    fi
echo ""

echo "<html><head><meta charset=\"utf-8\"></head><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</pre></body></html>" > "${PROGRESS}"
