#!/bin/bash
#SBATCH --job-name=exastencils_single_test_generated
#SBATCH -p idle
#SBATCH -A idle
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --exclusive
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=10


ID=${1}
BIN=${2}
EXP_RESULT=${3}
FAILURE_MAIL=${4}
LOG=${5}

RESULT="$(mktemp --tmpdir=/run/shm test_res_XXXXX.txt)"
FAILURE_SUBJECT="ExaStencils TestBot Error"

TIMEOUT=1


function cleanup {
  rm "${RESULT}" # do not remove ${BIN}, because job could be rescheduled, next time all tests are started, old binaries are removed anyway
  echo "        Removed  ${RESULT} (test id: '${ID}')" >> "${LOG}"
  if [[ ${TIMEOUT} -eq 1 ]]; then
    echo "========= FAILURE: ID '${ID}': Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (running testcode)." >> "${LOG}"
    echo "Test '${ID}' failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (running testcode)." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
  fi
}
trap cleanup EXIT

# ensure this script finishes with this function (even in case of an error) to prevent incorrect timeout error
function finish {
  TIMEOUT=0
  exit 0
}

echo "        Created  ${RESULT} (test id: '${ID}'): run code and redirect stdout and stderr" >> "${LOG}"

# run generated code
export OMPI_MCA_btl_tcp_if_include=132.231.64.0/23 # use infosun network only (hack to ensure all nodes can communicate via MPI)
srun "${BIN}" > "${RESULT}" 2>&1

if diff -B -w --strip-trailing-cr -I "time"  "${RESULT}" "${EXP_RESULT}" > /dev/null; then
  echo "          OK: ID '${ID}' on machine $(hostname) (${SLURM_JOB_NUM_NODES} nodes, ${SLURM_JOB_CPUS_PER_NODE} cores)." >> "${LOG}"
else
  echo "========= FAILED: ID '${ID}' on machine $(hostname) (${SLURM_JOB_NUM_NODES} nodes, ${SLURM_JOB_CPUS_PER_NODE} cores): invalid result." >> "${LOG}"
  echo "Test '${ID}' failed!  Results ($(basename ${RESULT})) do not match expected ones ($(basename ${EXP_RESULT}))." | mail -s "${FAILURE_SUBJECT}" -A "${RESULT}" -A "${EXP_RESULT}" ${FAILURE_MAIL}
fi

finish
