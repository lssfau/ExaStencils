#!/bin/bash
#SBATCH -p anywhere
#SBATCH -A anywhere
#SBATCH --qos=norm
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --mem=10G
#SBATCH --nice=100
#SBATCH --time=20
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


TESTING_DIR=${1}
COMPILER=${2}
MAIN=${3}
TEST_DIR=${4}
BIN=${5}
KNOWLEDGE=${6}
EXAFILES=${7}
PLATFORM=${8}
ERROR_MARKER=${9}
LOG_ALL=${10}
LINK=${11}
PROGRESS=${12}
BRANCH=${13}


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

echo "Generate and compile on machine ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."
echo ""
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

mkdir -p "${TEST_DIR}"
SETTINGS="${TEST_DIR}/settings.txt"


RESULT=$(mktemp --tmpdir=/dev/shm || mktemp --tmpdir=/tmp) || {
    echo "ERROR: Failed to create temporary file."
    touch ${ERROR_MARKER}
    echo "${LINK}" >> "${LOG_ALL}"
    exit 0
  }
EXPL_CFG=$(mktemp --tmpdir=/dev/shm || mktemp --tmpdir=/tmp) || {
    echo "ERROR: Failed to create temporary file."
    touch ${ERROR_MARKER}
    echo "${LINK}" >> "${LOG_ALL}"
    exit 0
  }
if [[ ! ${RESULT} =~ ^/dev/shm/* ]]; then
  echo "Problems with /dev/shm on machine ${SLURM_JOB_NODELIST} in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID}." | mail -s "ExaTest /dev/shm" "kronast@fim.uni-passau.de"
fi

function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  echo "${LINK}  (maybe requeued)" >> "${LOG_ALL}"
  exit 1
}
trap killed SIGTERM

STARTTIME=$(date +%s)

function cleanup {
  rm "${RESULT}" "${EXPL_CFG}"
  echo "  Removed  ${RESULT} and ${EXPL_CFG}"
  ENDTIME=$(date +%s)
  echo "Runtime: $((${ENDTIME} - ${STARTTIME})) seconds  (target code generation and compilation)"
  echo ""
  echo ""
  echo "-----------------------------------------------------------------------------------------------"
}
trap cleanup EXIT


# build settings file
rm -f "${SETTINGS}"
touch "${SETTINGS}"
echo "outputPath = \"${TEST_DIR}\"" >> "${SETTINGS}"
echo "binary = \"${BIN}\"" >> "${SETTINGS}"
echo "poly_explorationConfig = \"${EXPL_CFG}\"" >> "${SETTINGS}"
IFS=';' read -a files <<< $EXAFILES
for f in "${files[@]}"; do
  case ${f} in
    *.exa1)  echo "l1file = \"${f}\"" >> "${SETTINGS}";;
    *.exa2)  echo "l2file = \"${f}\"" >> "${SETTINGS}";;
    *.exa3)  echo "l3file = \"${f}\"" >> "${SETTINGS}";;
    *.exa4)  echo "l4file = \"${f}\"" >> "${SETTINGS}";;
  esac
done


echo "Run generator:"
echo "  Created  ${RESULT}: run generator and save its stdout and stderr."
cd ${TESTING_DIR}  # there is no possibility to explicitly set the working directory of the jvm... (changing property user.dir does not work in all situations)
set -o pipefail
srun --mem=10G java -XX:+UseG1GC -Xmx8G -cp "${COMPILER}" ${MAIN} "${SETTINGS}" "${KNOWLEDGE}" "${PLATFORM}" 2>&1 | sed 's|\(WARN:.*\)$|<span style="color: #FF8000">\1</span>|;s|\(ERROR:.*\)$|<span style="color: #E00000">\1</span>|;s|\(Exception in.*\)$|<span style="color: #E00000">\1</span>|;s|\(DBG:\s*Done!\)$|<span style="color: #00E000">\1</span>|' | tee "${RESULT}"
RETCODE=$?
    if grep -q "Bad file descriptor" ${RESULT}; then
      echo "restart generation..."
      cleanup # call cleanup directly; no exit trap when requeue is performed (slurm kills this script completly)
      scontrol requeue ${SLURM_JOB_ID}
      sleep 60 # ensure this execution never enters a finished state (for dependences), since scontrol might need some time
    fi
    if [[ ${RETCODE} -ne 0 ]]; then
      echo ""
      echo "<span style=\"color: #E00000\">ERROR: generator return code unequal to 0: ${RETCODE}.</span>"
      echo ""
      touch ${ERROR_MARKER}
      echo "${LINK}" >> "${LOG_ALL}"
      exit 1
    fi
echo ""
head -n 14 "${EXPL_CFG}"
echo ""
echo ""
echo "-----------------------------------------------------------------------------------------------"
# set environment for cuda compiler
if [[ ! ${PATH} =~ cuda ]]; then
  CUDA_PREFIX="/usr/local/cuda"
  if [[ -d "${CUDA_PREFIX}/bin" ]]; then
    export PATH="${CUDA_PREFIX}/bin:${PATH}"
  fi
  if [[ -d "${CUDA_PREFIX}/lib" ]]; then
    export LD_RUN_PATH="${CUDA_PREFIX}/lib:${LD_RUN_PATH}"
    export LD_LIBRARY_PATH="${CUDA_PREFIX}/lib:${LD_LIBRARY_PATH}"
    export LIBRARY_PATH="${CUDA_PREFIX}/lib:${LIBRARY_PATH}"
  fi
  if [[ -d "${CUDA_PREFIX}/lib64" ]]; then
    export LD_RUN_PATH="${CUDA_PREFIX}/lib64:${LD_RUN_PATH}"
    export LD_LIBRARY_PATH="${CUDA_PREFIX}/lib64:${LD_LIBRARY_PATH}"
    export LIBRARY_PATH="${CUDA_PREFIX}/lib64:${LIBRARY_PATH}"
  fi
  if [[ -d "${CUDA_PREFIX}/include" ]]; then
    export INCLUDE_PATH="${CUDA_PREFIX}/include:${INCLUDE_PATH}"
    export C_INCLUDE_PATH="${CUDA_PREFIX}/include:${C_INCLUDE_PATH}"
    export CPLUS_INCLUDE_PATH="${CUDA_PREFIX}/include:${CPLUS_INCLUDE_PATH}"
  fi
fi
echo "Call make:"
srun --mem=10G make -C "${TEST_DIR}" -j ${SLURM_CPUS_ON_NODE}
RETCODE=$?
    if [[ ${RETCODE} -ne 0 ]]; then
      echo ""
      echo "<span style=\"color: #E00000\">ERROR: generator return code unequal to 0: ${RETCODE}.</span>"
      echo ""
      touch ${ERROR_MARKER}
      echo "${LINK}" >> "${LOG_ALL}"
      exit 1
    fi
echo ""

update_progress 2
