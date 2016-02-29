#!/bin/bash
#SBATCH --job-name=et_git
#SBATCH -p anywhere
#SBATCH -A anywhere
#SBATCH --qos=norm
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
BRANCH=${7}
FORCE_START=${8}

REPO_DIR="${BASE_DIR}/repo"
SCR_DIR="${BASE_DIR}/scripts"
TEMP_DIR="${BASE_DIR}/temp/${BRANCH}"
FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_MAIL_SUBJECT="ExaStencils TestBot Error (cron)"

OUT_DIR=$(dirname "${OUT_FILE}")

GIT_URL="ssh://git@git.infosun.fim.uni-passau.de/exastencils/dev/ScalaExaStencil.git"


function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  error "Error"
}
trap killed SIGTERM

function cleanup {
  rm -f "${TMP_OUT_FILE}" "${TESTS_LOCK}"
}
trap cleanup EXIT

function error {
  echo "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">" > "${PROGRESS}"
  echo "${1}" >> "${PROGRESS}"
  echo "" >> "${PROGRESS}"
  echo "" >> "${PROGRESS}"
  cat "${TMP_OUT_FILE}"  >> "${PROGRESS}"
  echo "" >> "${PROGRESS}"
  echo "</div></body></html>" >> "${PROGRESS}"
  exit 1
}


echo "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">"
echo "$(date -R):  Initialize tests on host ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID}) for branch ${BRANCH}..."
echo "Progress can be found <a href=../$(basename ${PROGRESS})>here</a>.  (Reload page manually.)"
# echo "Progress can be found <a href=$(realpath --relative-to=${OUT_DIR} ${PROGRESS})>here</a>.  (Reload page manually.)" # FIXME: ubuntus realpath version is from 2011...
echo ""

STARTTIME=$(date +%s)

if [[ -d "${REPO_DIR}" ]]; then
  echo "Repo found, try to pull:"
  srun git -C "${REPO_DIR}" fetch --force
  srun git -C "${REPO_DIR}" checkout . # revert all (accidental) changes to files in repo (e.g. overwritten l4 files)
  srun git -C "${REPO_DIR}" clean -fxd # delete ALL untracked files
  srun git -C "${REPO_DIR}" checkout --force -B "${BRANCH}" --track "remotes/origin/${BRANCH}"
    if [[ $? -ne 0 ]]; then
      error "ERROR: switch to branch ${BRANCH} failed."
    fi
  OLD_HASH=$(git -C "${REPO_DIR}" rev-parse @)
  srun git -C "${REPO_DIR}" merge FETCH_HEAD
      if [[ $? -ne 0 ]]; then
        error "ERROR: git remote update failed."
      fi
  NEW_HASH=$(git -C "${REPO_DIR}" rev-parse @)
  if [[ "${FORCE_START}" -eq 0 && ${OLD_HASH} = ${NEW_HASH} ]]; then
    # up-to-date, no need to run tests, exit script
    if [[ -z "$(squeue -h -u exatest | grep -v ${SLURM_JOB_NAME})" ]]; then # only output log if there are no old tests running
      echo "$(date -R):  Tests triggered, but there are no new commits since last run, finish." >> "${OUT_FILE}"
      echo "<html><head><meta charset=\"utf-8\"></head><body><pre>$(date -R):  Done!</pre></body></html>" > "${PROGRESS}"
    fi
    exit 0
  fi
else
  echo "No local repo found, create a new clone:"
  mkdir -p "${REPO_DIR}"
  srun git clone "${GIT_URL}" "${REPO_DIR}" --branch "${BRANCH}"
      if [[ $? -ne 0 ]]; then
        error "ERROR: git clone failed."
      fi
fi

mkdir -p "${OUT_DIR}"
mkdir -p "${TEMP_DIR}"
NEW_HASH=$(git -C "${REPO_DIR}" rev-parse @)
echo ""
echo "Run tests for hash  ${NEW_HASH}."
if [[ -e "${REPO_DIR}/Testing/tests_version.txt" ]] && [[ $(cat "${SCR_DIR}/tests_version.txt") -lt $(cat "${REPO_DIR}/Testing/tests_version.txt") ]]; then
  rm "${SCR_DIR}"/*
  cp "${REPO_DIR}"/Testing/tests_version.txt "${SCR_DIR}"
  cp "${REPO_DIR}"/Testing/tests*.sh "${SCR_DIR}"
fi
(unset SLURM_JOB_NAME; sbatch -o "${OUT_FILE}" -e "${OUT_FILE}" "--dependency=afterok:${SLURM_JOB_ID}" "${SCR_DIR}/tests1_all.sh" "${SCR_DIR}" "${REPO_DIR}" "${BASE_DIR}/scala/" "${TEMP_DIR}" "${OUT_FILE}" "${OUT_FILE_URL}" "${PROGRESS}")
      if [[ $? -ne 0 ]]; then
        error "ERROR: Unable to enqueue testing job."
      fi
echo ""
echo ""

ENDTIME=$(date +%s)
echo "Runtime: $((${ENDTIME} - ${STARTTIME})) seconds  (git pull)"
echo ""
echo ""

rm -f "${OUT_DIR}"/*
echo "<html><head><meta charset=\"utf-8\"></head><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</pre></body></html>" > "${PROGRESS}"
srun mv "${TMP_OUT_FILE}" "${OUT_FILE}"
