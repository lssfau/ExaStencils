#!/bin/bash
#SBATCH --qos=norm
#SBATCH --hint=nomultithread
#SBATCH --nice=100
#SBATCH --time=15
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


BIN=${1}
EXP_RESULT=${2}
TEMP_DIR=${3}
ERROR_MARKER=${4}
LOG_ALL=${5}
LINK=${6}
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

update_progress 1

echo "Running test on machine(s) ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

RESULT=$(mktemp --tmpdir=${TEMP_DIR} test_res_XXXXXXXX.txt) || { # should not be placed in ram, since all nodes must have access to the same file
    echo "ERROR: Failed to create temporary file."
    touch ${ERROR_MARKER}
    echo "${LINK}" >> "${LOG_ALL}"
    exit 0
  }

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
  echo "<a href=./>Back to overview.</a>"
  echo ""
}
trap cleanup EXIT


# run generated code
echo "  Created  ${RESULT}: run code and redirect its stdout and stderr."
srun --cpu_bind=socket "${BIN}" 2>&1 | grep -v -e "No protocol specified" -e "fglrx" -e "srun: setscheduler: init" -e "NVIDIA: no NVIDIA devices found" -e "error stating /local/exatest in is_ssd" | tee "${RESULT}" # HACK: filter several strange error messages and warnings...
echo ""

if grep -q "Communication connection failure" ${RESULT}; then
  echo "restart test..."
  cleanup # call cleanup directly; no exit trap when requeue is performed (slurm kills this script completly)
  scontrol requeue ${SLURM_JOB_ID}
  sleep 60 # ensure this execution never enters a finished state (for dependences), since scontrol might need some time
fi

if diff -B -w --strip-trailing-cr -I "time" -I "No root privilege"  "${RESULT}" "${EXP_RESULT}" > /dev/null; then
  echo '<span style="color: #00E000"><b>============== Test OK ==============</b></span>'
else
  echo '<span style="color: #E00000"><b>============== Test ERROR ==============</b></span>'
  echo "invalid result, expected:"
  cat "${EXP_RESULT}"
  touch ${ERROR_MARKER}
  echo "${LINK}" >> "${LOG_ALL}"
fi
echo ""

update_progress 2
