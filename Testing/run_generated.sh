#!/bin/bash
#SBATCH --job-name=exastencils_single_test_generated
#SBATCH -p idle
#SBATCH -A idle
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=5


ID=${1}
BIN=${2}
EXP_RESULT=${3}
FAILURE_MAIL=${4}
LOG=${5}

RESULT="$(mktemp --tmpdir=/run/shm test_res_XXXXX.txt)"
FAILURE_SUBJECT="ExaStencils TestBot Error"

TIMEOUT=1


function finish {
  rm "${RESULT}" "${BIN}"
  echo "        Removed  ${RESULT} (test id: '${ID}')" >> "${LOG}"
  if [[ ${TIMEOUT} -eq 1 ]]; then
    echo "========= FAILURE: ID '${ID}': Timeout when calling ${BASH_SOURCE} (running testcode)" >> "${LOG}"
	echo "Test '${ID}' failed!  Timeout when calling ${BASH_SOURCE} (running testcode)" | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
  fi
}
trap finish EXIT

echo "        Created  ${RESULT} (test id: '${ID}'): run code and redirect stdout and stderr" >> "${LOG}"

# run generated code
srun "${BIN}" > "${RESULT}" 2>&1

if diff -B -w --strip-trailing-cr -I "time"  "${RESULT}" "${EXP_RESULT}" > /dev/null; then
  echo "          OK: ID '${ID}' on machine $(hostname) (${SLURM_JOB_NUM_NODES} nodes, ${SLURM_JOB_CPUS_PER_NODE} cores)." >> "${LOG}"
else
  echo "========= FAILED: ID '${ID}' on machine $(hostname) (${SLURM_JOB_NUM_NODES} nodes, ${SLURM_JOB_CPUS_PER_NODE} cores): invalid result." >> "${LOG}"
  echo "Test '${ID}' failed!  Results ($(basename ${RESULT})) do not match expected ones ($(basename ${EXP_RESULT}))." | mail -s "${FAILURE_SUBJECT}" -A "${RESULT}" -A "${EXP_RESULT}" ${FAILURE_MAIL}
fi

TIMEOUT=0
