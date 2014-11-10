#!/bin/bash
#SBATCH --job-name=exastencils_single_test
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=10


BASE_DIR=${1} # testing directory
ID=${2}
COMPILER=${3}
MAIN=${4}
KNOWLEDGE=${5}
FAILURE_MAIL=${6}
LOG=${7}
EXP_RESULT=${8}
NODES=${9}
CORES=${10}
CONSTRAINTS=${11}

FAILURE_SUBJECT="ExaStencils TestBot Error"

TMP_DIR="$(mktemp --tmpdir=/run/shm -d)"
OUTPUT="${TMP_DIR}/command_output.txt"
SETTINGS="${TMP_DIR}/settings.txt"
L4="${TMP_DIR}/l4.exa"
BIN="$exastencils_${SLURM_JOB_ID}"

TIMEOUT=1


function cleanup {
  rm -rf "${TMP_DIR}"
  echo "      Removed  ${TMP_DIR} (test id: '${ID}')" >> "${LOG}"
  if [[ ${TIMEOUT} -eq 1 ]]; then
    echo "===== FAILURE: ID '${ID}': Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (generate and compile test)." >> "${LOG}"
    echo "Test '${ID}' failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (generate and compile test)." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
  fi
}
trap cleanup EXIT

# ensure this script finishes with this function (even in case of an error) to prevent incorrect timeout error
function finish {
  TIMEOUT=0
  exit 0
}

echo "      Created  ${TMP_DIR} (test id: '${ID}'): application build dir" >> "${LOG}"

# build settings file
touch "${SETTINGS}"
echo "outputPath = \"${TMP_DIR}\"" >> "${SETTINGS}"
echo "l4file = \"${L4}\"" >> "${SETTINGS}"
echo "binary = \"${BIN}\"" >> "${SETTINGS}"

cd ${BASE_DIR}  # there is no possibility to explicitly set the working directory of the jvm... (changing property user.dir does not work in all situations)
srun java -cp "${COMPILER}" ${MAIN} "${SETTINGS}" "${KNOWLEDGE}" > "${OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "===== FAILED: ID '${ID}': generator error." >> "${LOG}"
      echo "Test '${ID}' failed!  Unable to generate code." | mail -s "${FAILURE_SUBJECT}" -A "${OUTPUT}" ${FAILURE_MAIL}
      finish
    fi
srun make -C "${TMP_DIR}" -j ${SLURM_CPUS_ON_NODE} > "${OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "===== FAILED: ID '${ID}': target compiler error." >> "${LOG}"
      echo "Test '${ID}' failed!  Unable to compile target code." | mail -s "${FAILURE_SUBJECT}" -A "${OUTPUT}" ${FAILURE_MAIL}
      finish
    fi

if [[ ${CORES} = "" ]]; then
  echo "          OK: ID '${ID}' (not executed)." >> "${LOG}"
else
  cp "${TMP_DIR}/${BIN}" "${BASE_DIR}/${BIN}" # store in NFS, as testrun could be enqueued on a different machine
  sbatch -n ${NODES} -c ${CORES} --constraint="${CONSTRAINTS}" "${BASE_DIR}/run_generated.sh" ${ID} "${BASE_DIR}/${BIN}" "${EXP_RESULT}" "${FAILURE_MAIL}" "${LOG}"
      if [[ $? -ne 0 ]]; then
        echo "===== FAILED: ID '${ID}': unable to enqueue job  (nodes: ${NODES},  cores: ${CORES},  constraints: '${CONSTRAINTS}')." >> "${LOG}"
        echo "Test '${ID}' failed!  Unable to enqueue job  (nodes: ${NODES},  cores: ${CORES},  constraints: '${CONSTRAINTS}')." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
        finish
      fi
fi

finish
