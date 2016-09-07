#!/bin/bash
#SBATCH --job-name=et_logs
#SBATCH -p anywhere
#SBATCH -A anywhere
#SBATCH --qos=norm
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
BRANCH=${8}


function update_progress {
  if [[ "${1}" -eq 0 ]]; then
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)  (Reload this page manually.)\n Log can be found <a href=./${BRANCH}/>here</a>.\n\n  Done!\n\n  New tests can be triggered <a href=../trigger-eg-tests.html>here</a></div></body></html>" > "${PROGRESS}"
  elif [[ "${1}" -eq 1 ]]; then
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)  (Reload this page manually.)\n Log can be found <a href=./${BRANCH}/>here</a>.\n\n$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</div></body></html>" > "${PROGRESS}"
  else
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)  (Reload this page manually.)\n Log can be found <a href=./${BRANCH}/>here</a>.\n\n$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R" | grep -v ${SLURM_JOB_ID})</div></body></html>" > "${PROGRESS}"
  fi
}

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
  srun 7z a "${ERROR_ARCHIVE}" ${TO_ZIP}
  echo ""
  echo ""
  echo "Errors in automatic tests!  See log (or attachment) for details: ${OUT_FILE_URL}" | mail -s "TestBot Error" -A "${ERROR_ARCHIVE}" ${FAILURE_MAIL}
fi

echo "Tests finished at $(date -R)."
echo "New tests can be triggered <a href=../../trigger-eg-tests.html>here</a>."
echo ""
echo "============================================================================"

update_progress 0
