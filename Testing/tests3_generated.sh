#!/bin/bash
#SBATCH --job-name=exatest_generated
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --exclusive
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=15
#SBATCH --signal=INT@5


ID=${1}
BIN=${2}
EXP_RESULT=${3}
FAILURE_MAIL=${4}
FAILURE_MAIL_SUBJECT=${5}
LOG=${6}

RESULT="$(mktemp --tmpdir=/run/shm test_res_XXXXX.txt)" || {
    echo "========= FAILURE: ID '${ID}': Failed to create temporary file on machine $(hostname) in ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (running testcode)." >> "${LOG}"
    echo "Test '${ID}' failed!  Unable to create temporary file in ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (running testcode)." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
    exit 0
  }


function killed {
  echo "          ??? ID '${ID}': Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)  (running testcode)." >> "${LOG}"
  exit 0
}
trap killed SIGTERM

function cleanup {
  rm "${RESULT}" # do not remove ${BIN} since job could be rescheduled; next time all tests are started old binaries are removed anyway
  echo "        Removed  ${RESULT} (test id: '${ID}')" >> "${LOG}"
}
trap cleanup EXIT


echo "        Created  ${RESULT} (test id: '${ID}'): run code and redirect stdout and stderr" >> "${LOG}"

# run generated code
srun "${BIN}" > "${RESULT}" 2>&1

if diff -B -w --strip-trailing-cr -I "time"  "${RESULT}" "${EXP_RESULT}" > /dev/null; then
  echo "          OK: ID '${ID}' on machine $(hostname) (${SLURM_JOB_NUM_NODES} nodes, ${SLURM_JOB_CPUS_PER_NODE} cores)." >> "${LOG}"
else
  echo "========= FAILED: ID '${ID}' on machine $(hostname) (${SLURM_JOB_NUM_NODES} nodes, ${SLURM_JOB_CPUS_PER_NODE} cores): invalid result." >> "${LOG}"
  echo "Test '${ID}' failed!  Results ($(basename ${RESULT})) do not match expected ones ($(basename ${EXP_RESULT}))." | mail -s "${FAILURE_MAIL_SUBJECT}" -A "${RESULT}" -A "${EXP_RESULT}" ${FAILURE_MAIL}
fi
