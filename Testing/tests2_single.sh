#!/bin/bash
#SBATCH --job-name=exatest_single
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=10
#SBATCH --signal=10@5


TESTING_DIR=${1}
BIN_DIR=${2}
ID=${3}
COMPILER=${4}
MAIN=${5}
KNOWLEDGE=${6}
FAILURE_MAIL=${7}
FAILURE_MAIL_SUBJECT=${8}
LOG=${9}
EXP_RESULT=${10}
NODES=${11}
CORES=${12}
CONSTRAINTS=${13}

RAM_TMP_DIR="$(mktemp --tmpdir=/run/shm -d)" || {
    echo "===== FAILURE: ID '${ID}': Failed to create temporary directory on machine $(hostname) in ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (generate and compile test)." >> "${LOG}"
    echo "Test '${ID}' failed!  Unable to create temporary directory in ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (generate and compile test)." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
    exit 0
  }
OUTPUT="${RAM_TMP_DIR}/command_output.txt"
SETTINGS="${RAM_TMP_DIR}/settings.txt"
L4="${RAM_TMP_DIR}/l4.exa"
BIN="exastencils_${ID}_${SLURM_JOB_ID}"


function timeout {
  echo "===== FAILURE: ID '${ID}': Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (generate and compile test)." >> "${LOG}"
  echo "Test '${ID}' failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (generate and compile test)." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
  exit 0
}
trap timeout 10

function killed {
  echo "      ??? ID '${ID}': Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is requeued)  (generate and compile test)." >> "${LOG}"
  exit 0
}
trap killed SIGTERM

function cleanup {
  rm -rf "${RAM_TMP_DIR}"
  echo "      Removed  ${RAM_TMP_DIR} (test id: '${ID}')" >> "${LOG}"
}
trap cleanup EXIT


echo "      Created  ${RAM_TMP_DIR} (test id: '${ID}'): application build dir" >> "${LOG}"

# build settings file
touch "${SETTINGS}"
echo "outputPath = \"${RAM_TMP_DIR}\"" >> "${SETTINGS}"
echo "l4file = \"${L4}\"" >> "${SETTINGS}"
echo "binary = \"${BIN}\"" >> "${SETTINGS}"

touch "${OUTPUT}"
pushd ${TESTING_DIR}  # there is no possibility to explicitly set the working directory of the jvm... (changing property user.dir does not work in all situations)
srun java -cp "${COMPILER}" ${MAIN} "${SETTINGS}" "${KNOWLEDGE}" > "${OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "===== FAILED: ID '${ID}': generator error." >> "${LOG}"
      echo "Test '${ID}' failed!  Unable to generate code." | mail -s "${FAILURE_MAIL_SUBJECT}" -A "${OUTPUT}" ${FAILURE_MAIL}
      exit 0
    fi
popd
srun make -C "${RAM_TMP_DIR}" -j ${SLURM_CPUS_ON_NODE} > "${OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "===== FAILED: ID '${ID}': target compiler error." >> "${LOG}"
      echo "Test '${ID}' failed!  Unable to compile target code." | mail -s "${FAILURE_MAIL_SUBJECT}" -A "${OUTPUT}" ${FAILURE_MAIL}
      exit 0
    fi

if [[ ${CORES} = "" ]]; then
  echo "          OK: ID '${ID}' (not executed)." >> "${LOG}"
else
  cp "${RAM_TMP_DIR}/${BIN}" "${BIN_DIR}/${BIN}" # store in NFS, as testrun could be enqueued on a different machine
  ACC="idle"
  PART="idle"
  CONSTR_PARAM="--constraint=${CONSTRAINTS}"
  if [[ $(( ${NODES} * ${CORES} )) -gt 30 ]] || [[ ${CONSTRAINTS} =~ "E5" ]]; then # HACK to ensure jobs are executed in a reasonable time
    ACC="cl"
    PART="chimaira"
    CONSTR_PARAM=""
  fi
  sbatch -A ${ACC} -p ${PART} -n ${NODES} -c ${CORES} ${CONSTR_PARAM} "${TESTING_DIR}/tests3_generated.sh" ${ID} "${BIN_DIR}/${BIN}" "${EXP_RESULT}" ${FAILURE_MAIL} "${FAILURE_MAIL_SUBJECT}" "${LOG}"
      if [[ $? -ne 0 ]]; then
        echo "===== FAILED: ID '${ID}': Unable to enqueue job  (account: ${ACC},  partition: ${PART},  nodes: ${NODES},  cores: ${CORES},  constraints: '${CONSTRAINTS}')." >> "${LOG}"
        echo "Test '${ID}' failed!  Unable to enqueue job  (account: ${ACC},  partition: ${PART},  nodes: ${NODES},  cores: ${CORES},  constraints: '${CONSTRAINTS}')." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
      fi
fi
