#!/bin/bash
#SBATCH --job-name=et_logs
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 1
#SBATCH --nice=100
#SBATCH --time=5
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


FAILURE_MAIL=${1}
OUT_FILE=${2} # stdout and stderr should already be redirected to this file
OUT_FILE_URL=${3} # url to ${OUT_FILE}
ERROR_MARKER_NAME=${4}
ERROR_MARKER=${5}
LOG_DIR=${6}
PROGRESS=${7}


echo ""
echo "-----------------------------------------------------"
echo "Evaluating results on machine ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."

function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  exit 1
}
trap killed SIGTERM


TO_ZIP=""

if [[ -f "${ERROR_MARKER}" ]]; then
  cp "${OUT_FILE}" "${LOG_DIR}/tests.log"
  TO_ZIP="${TO_ZIP} tests.log"
fi

cd "${LOG_DIR}"
for log in $(ls *.html); do
  TEST_LOG="${LOG_DIR}/${log}"
  TEST_ERROR_MARKER="${TEST_LOG}.${ERROR_MARKER_NAME}"
  if [[ -f "${TEST_ERROR_MARKER}" ]]; then
    rm "${TEST_ERROR_MARKER}"
    TO_ZIP="${TO_ZIP} ${log}"
  fi
done

echo ""
echo ""
if [[ -n "${TO_ZIP}" ]]; then
  ERROR_ARCHIVE="${LOG_DIR}/ErrorLogs.7z"
  7z a "${ERROR_ARCHIVE}" ${TO_ZIP}
  echo ""
  echo ""
  echo "Errors in automatic tests!  See log or attachment for details: ${OUT_FILE_URL}" | mail -s "TestBot Error" -A "${ERROR_ARCHIVE}" ${FAILURE_MAIL}
fi

echo "Tests finished at $(date -R)."
echo ""
echo "============================================================================"

echo "<html><body><pre>$(date -R):  Done!</pre></body></html>" > "${PROGRESS}"
