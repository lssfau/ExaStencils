#!/bin/bash
#SBATCH --job-name=et_git
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 1
#SBATCH --time=5
#SBATCH --signal=INT@5
#SBATCH --open-mode=truncate


BASE_DIR=${1}
TMP_OUT_FILE=${2} # stdout and stderr should already be redirected to this file
OUT_FILE=${3}
OUT_FILE_URL=${4} # url to ${OUT_FILE}
PROGRESS=${5}
TESTS_LOCK=${6}
FORCE_START=${7}

REPO_DIR="${BASE_DIR}/repo"
TEMP_DIR="${BASE_DIR}/temp"
FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_MAIL_SUBJECT="ExaStencils TestBot Error (cron)"

OUT_DIR=$(dirname "${OUT_FILE}")

GIT_URL="ssh://git@git.infosun.fim.uni-passau.de/exastencils/dev/ScalaExaStencil.git"


function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  exit 1
}
trap killed SIGTERM

function cleanup {
  rm "${TMP_OUT_FILE}" "${TESTS_LOCK}"
}
trap cleanup EXIT


echo "<html><head><meta charset="utf-8"></head><body><div style="white-space: pre-wrap; font-family:monospace;">"
echo "$(date -R):  Initialize tests on host ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})..."
echo "Progress can be found <a href=$(basename ${PROGRESS})>here</a>.  (Reload page manually.)"
echo ""

STARTTIME=$(date +%s)

if [[ -d "${REPO_DIR}" ]]; then
  OLD_HASH=$(git -C "${REPO_DIR}" rev-parse @)
  echo "Repo found, try to pull:"
  srun git -C "${REPO_DIR}" pull --force
      if [[ $? -ne 0 ]]; then
        echo "ERROR: git remote update failed."
        echo "git remote update failed." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
        exit 1
      fi
  NEW_HASH=$(git -C "${REPO_DIR}" rev-parse @)
  if [[ -z "${FORCE_START}" && ${OLD_HASH} = ${NEW_HASH} ]]; then
    # up-to-date, no need to run tests, exit script
    if [[ -z "$(squeue -h -u exatest | grep -v ${SLURM_JOB_NAME})" ]]; then # only output log if there are no old tests running
      echo "$(date -R):  Tests triggered, but there are no new commits since last run, finish." >> "${OUT_FILE}"
      echo "<html><head><meta charset="utf-8"></head><body><pre>$(date -R):  Done!</pre></body></html>" > "${PROGRESS}"
    fi
    exit 0
  fi
else
  echo "No local repo found, create a new clone:"
  mkdir -p "${REPO_DIR}"
  srun git clone "${GIT_URL}" "${REPO_DIR}"
      if [[ $? -ne 0 ]]; then
        echo "ERROR: git clone failed."
        echo "git clone failed." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
        exit 1
      fi
fi

mkdir -p "${TEMP_DIR}"
NEW_HASH=$(git -C "${REPO_DIR}" rev-parse @)
echo ""
echo "Run tests for hash  ${NEW_HASH}."
(unset SLURM_JOB_NAME; sbatch -o "${OUT_FILE}" -e "${OUT_FILE}" "--dependency=afterok:${SLURM_JOB_ID}" "${REPO_DIR}/Testing/tests1_all.sh" "${REPO_DIR}" "${BASE_DIR}/scala/" "${TEMP_DIR}" "${OUT_FILE}" "${OUT_FILE_URL}" "${PROGRESS}")
      if [[ $? -ne 0 ]]; then
        echo "ERROR: Unable to enqueue testing job."
        echo "Test failed!  Unable to enqueue testing job." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
      fi
echo ""
echo ""

ENDTIME=$(date +%s)
echo "Runtime: $((${ENDTIME} - ${STARTTIME})) seconds  (git pull)"
echo ""
echo ""

rm -f "${OUT_DIR}"/*
echo "<html><head><meta charset="utf-8"></head><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</pre></body></html>" > "${PROGRESS}"
cat "${TMP_OUT_FILE}" > "${OUT_FILE}"
