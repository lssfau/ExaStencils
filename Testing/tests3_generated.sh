#!/bin/bash
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --nice=100
#SBATCH --time=15
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


BIN=${1}
EXP_RESULT=${2}
ERROR_MARKER=${3}
LOG_ALL=${4}
LINK=${5}
PROGRESS=${6}


echo "<html><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</pre></body></html>" > "${PROGRESS}"

echo "Running test on machine(s) ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

RESULT=$(mktemp --tmpdir=/run/shm test_res_XXXXX.txt || mktemp --tmpdir=/tmp test_res_XXXXX.txt) || {
    echo "ERROR: Failed to create temporary file."
    touch ${ERROR_MARKER}
    echo "${LINK}" >> "${LOG_ALL}"
    exit 0
  }
if [[ ! ${RAM_TMP_DIR} =~ ^/run/shm/* ]]; then
  echo "Problems with /run/shm on machine ${SLURM_JOB_NODELIST} in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID}." | mail -s "ExaTest /run/shm" "kronast@fim.uni-passau.de"
fi

function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  echo "${LINK}  (maybe requeued)" >> "${LOG_ALL}"
  exit 0
}
trap killed SIGTERM

STARTTIME=$(date +%s)

function cleanup {
  rm "${RESULT}" # do not remove ${BIN} since job could be requeued; next time all tests are started old binaries are removed anyway
  echo "  Removed  ${RESULT}"
  echo ""
  ENDTIME=$(date +%s)
  echo "Runtime: $((${ENDTIME} - ${STARTTIME})) seconds  (test execution)"
}
trap cleanup EXIT


# run generated code
echo "  Created  ${RESULT}: run code and redirect its stdout and stderr."
srun "${BIN}" 2>&1 | grep -v "No protocol specified" > "${RESULT}" # HACK: filter strange X server error...
cat "${RESULT}"
echo ""

if diff -B -w --strip-trailing-cr -I "time"  "${RESULT}" "${EXP_RESULT}" > /dev/null; then
  echo "============== Test OK =============="
else
  echo "============== Test ERROR =============="
  echo "invalid result, expected:"
  cat "${EXP_RESULT}"
  touch ${ERROR_MARKER}
  echo "${LINK}" >> "${LOG_ALL}"
fi
echo ""
echo "<html><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</pre></body></html>" > "${PROGRESS}"
