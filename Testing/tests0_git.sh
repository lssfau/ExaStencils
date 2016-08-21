#!/bin/bash
#SBATCH --job-name=et_git
#SBATCH -p anywhere
#SBATCH -A anywhere
#SBATCH --qos=norm
#SBATCH -n 1
#SBATCH -c 1
#SBATCH --time=5
#SBATCH --signal=INT@5
#SBATCH -o /dev/null
#SBATCH -e /dev/null


BASE_DIR=${1}
OUT_FILE=${2}
OUT_FILE_URL=${3} # url to ${OUT_FILE}
PROGRESS=${4}
TESTS_LOCK=${5}
BRANCH=${6}
FORCE_START=${7}
FAILURE_MAIL=${8}

REPO_DIR="${BASE_DIR}/repo"
SCR_DIR="${BASE_DIR}/scripts"
TEMP_DIR="${BASE_DIR}/temp/${BRANCH}"
THIS_FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_MAIL_SUBJECT="ExaStencils TestBot Error (cron)"

TMP_OUT_FILE=$(mktemp --tmpdir=/run/shm 2>&1 || mktemp --tmpdir=/tmp 2>&1) || {
    echo -e "ERROR: Failed to create temporary file.\n\n${TMP_OUT_FILE}" | mail -s "${FAILURE_MAIL_SUBJECT}" ${THIS_FAILURE_MAIL}
    exit 0
  }
if [[ ! ${TMP_OUT_FILE} =~ ^/run/shm/* ]]; then
  echo "Problems with /run/shm on machine ${SLURM_JOB_NODELIST} in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID}." | mail -s "ExaTest /run/shm" "kronast@fim.uni-passau.de"
fi
exec > ${TMP_OUT_FILE} 2>&1 # redirect any output using bash; don't use slurms output mechanism, since there is no guarantee all output was writte to the file when it is read at the end of this script

OUT_DIR=$(dirname "${OUT_FILE}")

GIT_URL="ssh://git@git.infosun.fim.uni-passau.de/exastencils/dev/ScalaExaStencil.git"

function update_progress {
  if [[ "${1}" -eq 0 ]]; then
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)\n Log can be found <a href=./${BRANCH}/>here</a>.  (Reload page manually.)\n\n  Done!</div></body></html>" > "${PROGRESS}"
  elif [[ "${1}" -eq 1 ]]; then
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)\n Log can be found <a href=./${BRANCH}/>here</a>.  (Reload page manually.)\n\n$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</div></body></html>" > "${PROGRESS}"
  else
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)\n Log can be found <a href=./${BRANCH}/>here</a>.  (Reload page manually.)\n\n$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R" | grep -v ${SLURM_JOB_ID})</div></body></html>" > "${PROGRESS}"
  fi
}


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
  if [[ ! "$(srun git -C "${REPO_DIR}" branch -a)" =~ "${BRANCH}" ]]; then
    error "ERROR: branch ${BRANCH} does not exist"
  fi
  OLD_HASH=$(srun git -C "${REPO_DIR}" rev-parse "${BRANCH}")
  srun git -C "${REPO_DIR}" checkout "${BRANCH}"
    if [[ $? -ne 0 ]]; then
      error "ERROR: switch to branch ${BRANCH} failed."
    fi
  srun git -C "${REPO_DIR}" rebase
      if [[ $? -ne 0 ]]; then
        error "ERROR: git remote update failed."
      fi
  NEW_HASH=$(srun git -C "${REPO_DIR}" rev-parse @)
  OLD_JOBS="$(squeue -h -u exatest -o %i | grep -v ${SLURM_JOB_ID})"
  if [[ "${FORCE_START}" -eq 1 ]]; then
    echo "Force flag given, start tests."
  elif [[ -n "${OLD_JOBS}" ]]; then
    exit 0
  elif [[ ${OLD_HASH} = ${NEW_HASH} ]]; then
    # up-to-date, no need to run tests, exit script
    if [[ -z "${OLD_JOBS}" ]]; then # only output log if there are no old tests running
      echo "$(date -R):  Tests triggered, but there are no new commits since last run, finish." >> "${OUT_FILE}"
      update_progress 0
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
NEW_HASH=$(srun git -C "${REPO_DIR}" rev-parse @)
echo ""
echo "Run tests for hash  ${NEW_HASH}."
if [[ -e "${REPO_DIR}/Testing/tests_version.txt" ]] && [[ $(cat "${SCR_DIR}/tests_version.txt") -lt $(cat "${REPO_DIR}/Testing/tests_version.txt") ]]; then
  rm "${SCR_DIR}"/*
  cp "${REPO_DIR}"/Testing/tests_version.txt "${SCR_DIR}"
  cp "${REPO_DIR}"/Testing/tests*.sh "${SCR_DIR}"
fi
(unset SLURM_JOB_NAME; sbatch -o "${OUT_FILE}" -e "${OUT_FILE}" "--dependency=afterok:${SLURM_JOB_ID}" "${SCR_DIR}/tests1_all.sh" "${SCR_DIR}" "${REPO_DIR}" "${BASE_DIR}/scala/" "${TEMP_DIR}" "${OUT_FILE}" "${OUT_FILE_URL}" "${PROGRESS}" "${BRANCH}" "${FAILURE_MAIL}")
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
update_progress 2
cat "${TMP_OUT_FILE}" > "${OUT_FILE}"
