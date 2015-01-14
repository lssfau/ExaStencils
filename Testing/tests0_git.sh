#!/bin/bash
#SBATCH --job-name=exatests
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 1
#SBATCH --time=5
#SBATCH --signal=INT@5
#SBATCH --open-mode=truncate


BASE_DIR=${1}
OUT_FILE=${2} # stdout and stderr should already be redirected to this file
OUT_FILE_URL=${3} # url to ${OUT_FILE}

REPO_DIR="${BASE_DIR}/repo"
TEMP_DIR="${BASE_DIR}/temp"
FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_MAIL_SUBJECT="ExaStencils TestBot Error (cron)"

GIT_URL="ssh://git@git.infosun.fim.uni-passau.de/exastencils/dev/ScalaExaStencil.git"


function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  exit 1
}
trap killed SIGTERM


echo "$(date -R):  Initialize tests on host ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})..."
echo ""

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
  if [[ ${OLD_HASH} = ${NEW_HASH} ]]; then
    # up-to-date, no need to run tests, exit script
    echo "No changes, finish."
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
(sbatch -o "${OUT_FILE}" -e "${OUT_FILE}" "${REPO_DIR}/Testing/tests1_all.sh" "${REPO_DIR}" "${TEMP_DIR}" "${OUT_FILE}" "${OUT_FILE_URL}")
      if [[ $? -ne 0 ]]; then
        echo "ERROR: Unable to enqueue testing job."
        echo "Test failed!  Unable to enqueue testing job." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
      fi
echo ""
echo ""
