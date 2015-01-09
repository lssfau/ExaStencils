#!/bin/bash
#SBATCH --job-name=exatest_generated
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --exclusive
#SBATCH --time=15
#SBATCH --signal=INT@5


BIN=${1}
EXP_RESULT=${2}
ERROR_MARKER=${3}

echo "Running test on machine(s) ${SLURM_JOB_NODELIST}."
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

RESULT="$(mktemp --tmpdir=/run/shm test_res_XXXXX.txt)" || {
    echo "ERROR: Failed to create temporary file."
    touch ${ERROR_MARKER}
    exit 0
  }


function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  exit 0
}
trap killed SIGTERM

function cleanup {
  rm "${RESULT}" # do not remove ${BIN} since job could be requeued; next time all tests are started old binaries are removed anyway
  echo "Removed  ${RESULT}"
}
trap cleanup EXIT


echo "Created  ${RESULT}: run code and redirect its stdout and stderr"

# run generated code
srun "${BIN}" 2>&1 | grep -v "No protocol specified" > "${RESULT}" # HACK: filter strange X server error...

if diff -B -w --strip-trailing-cr -I "time"  "${RESULT}" "${EXP_RESULT}" > /dev/null; then
  echo "Test OK" >> "${LOG}"
else
  echo "ERROR: invalid result:"
  cat "${RESULT}"
  echo ""
  echo "expected:"
  cat "${EXP_RESULT}"
  touch ${ERROR_MARKER}
fi
