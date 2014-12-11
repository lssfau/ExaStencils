#!/bin/bash
#SBATCH --job-name=exatests
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=5
#SBATCH --signal=10@5


BASE_DIR=${1}
LOG=${2}

REPO_DIR="${BASE_DIR}/repo"
TEMP_DIR="${BASE_DIR}/temp"
FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_MAIL_SUBJECT="ExaStencils TestBot Error (cron)"

GIT_URL="ssh://git@git.infosun.fim.uni-passau.de/exastencils/dev/ScalaExaStencil.git"


function timeout {
  echo "= FAILURE: Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (git checkout/update)." >> "${LOG}"
  echo "Test '${ID}' failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (git checkout/update)." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
  exit 0
}
trap timeout 10

function killed {
  echo "  ??? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is requeued)  (git checkout/update)." >> "${LOG}"
  exit 0
}
trap killed SIGTERM


# trim log
if [[ -c "${LOG}" ]]; then
  cat ${LOG} | tail -n 2000 > ${LOG}
fi

echo "--------------------------------------------" >> "${LOG}"
echo "$(date -R):  Initialize tests..." >> "${LOG}"

if [[ -d "${REPO_DIR}" ]]; then
  OLD_HASH=$(git -C "${REPO_DIR}" rev-parse @)
  srun git -C "${REPO_DIR}" pull --force
      if [[ $? -ne 0 ]]; then
        echo "= git remote update failed." >> "${LOG}"
        echo "git remote update failed." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
        exit 0
      fi
  NEW_HASH=$(git -C "${REPO_DIR}" rev-parse @)
  if [[ ${OLD_HASH} = ${NEW_HASH} ]]; then
    # up-to-date, no need to run tests, exit script
    echo "  No changes since last test runs, finishing." >> "${LOG}"
    exit 0
  fi
else
  echo "  No local repo found, create a new clone." >> "${LOG}"
  mkdir -p "${REPO_DIR}"
  srun git clone "${GIT_URL}" "${REPO_DIR}"
      if [[ $? -ne 0 ]]; then
        echo "= git clone failed." >> "${LOG}"
        echo "git clone failed." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
        exit 0
      fi
fi

mkdir -p "${TEMP_DIR}"
NEW_HASH=$(git -C "${REPO_DIR}" rev-parse @)
echo "  Run tests for hash  ${NEW_HASH}." >> "${LOG}"
sbatch "${REPO_DIR}/Testing/tests1_all.sh" "${REPO_DIR}" "${TEMP_DIR}" "${LOG}"
      if [[ $? -ne 0 ]]; then
        echo "= FAILED: Unable to enqueue testing job." >> "${LOG}"
        echo "Test failed!  Unable to enqueue testing job." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
      fi
