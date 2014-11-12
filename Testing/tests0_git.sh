#!/bin/bash
#SBATCH --job-name=exatest_git
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 1
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=5


BASE_DIR=${1}

REPO_DIR="${BASE_DIR}/repo"
TEMP_DIR="${BASE_DIR}/temp"
LOG="${BASE_DIR}/tests.log"
FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_SUBJECT="ExaStencils TestBot Error (cron)"

TIMEOUT=1


function cleanup {
  if [[ ${TIMEOUT} -eq 1 ]]; then
    echo "= FAILURE: Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (git checkout/update)" >> "${LOG}"
    echo "Automatic tests failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (git checkout/update)" | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
  fi
}
trap cleanup EXIT

# ensure this script finishes with this function (even in case of an error) to prevent incorrect timeout error
function finish {
  TIMEOUT=0
  exit 0
}

# trim log
if [[ -c "${LOG}" ]]; then
  cat ${LOG} | tail -n 2000 > ${LOG}
fi

echo "--------------------------------------------" >> "${LOG}"
echo "$(date -R):  Initialize tests..." >> "${LOG}"

GIT_URL="ssh://git@git.infosun.fim.uni-passau.de/exastencils/dev/ScalaExaStencil.git"
if [[ -d "${REPO_DIR}" ]]; then
  srun git -C "${REPO_DIR}" fetch "${GIT_URL}"
      if [[ $? -ne 0 ]]; then
        echo "  git remote update failed." >> "${LOG}"
        echo "git remote update failed." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
        finish
      fi
  if [[ $(git -C ${REPO_DIR} rev-parse @) = $(git -C ${REPO_DIR} rev-parse @{u}) ]]; then
    # up-to-date, no need to run tests, exit script
    echo "  No changes since last test runs, finishing." >> "${LOG}"
    finish
  else
    echo "  New commits found, pull" >> "${LOG}"
    srun git -C "${REPO_DIR}" pull --force "${GIT_URL}"
  fi
else
  echo "  No local repo found, create a new clone." >> "${LOG}"
  mkdir -p "${REPO_DIR}"
  srun git clone "${GIT_URL}" "${REPO_DIR}"
      if [[ $? -ne 0 ]]; then
        echo "  git clone failed." >> "${LOG}"
        echo "git clone failed." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
        finish
      fi
fi

mkdir -p "${TEMP_DIR}"
sbatch "${REPO_DIR}/Testing/tests1_all.sh" "${REPO_DIR}" "${TEMP_DIR}" "${LOG}"
finish
