#!/bin/bash
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --time=15
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


TESTING_DIR=${1}
COMPILER=${2}
MAIN=${3}
BIN=${4}
KNOWLEDGE=${5}
ERROR_MARKER=${6}
LOG_ALL=${7}
LINK=${8}
PROGRESS=${9}


echo "<html><body><pre>\n$(squeue -u ${USER} -o \"%.11i %10P %25j %3t %.5D %R\")\n</pre></body></html>" > "${PROGRESS}"

echo "<html><body><pre>"
echo "Generate and compile on machine ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."
echo ""
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

RAM_TMP_DIR="$(mktemp --tmpdir=/run/shm -d)" || {
    echo "ERROR: Failed to create temporary directory."
    touch ${ERROR_MARKER}
    echo "${LINK}" >> "${LOG_ALL}"
    exit 1
  }
SETTINGS="${RAM_TMP_DIR}/settings.txt"
L4="${RAM_TMP_DIR}/l4.exa"
TMP_BIN="exastencils"


function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  echo "${LINK}" >> "${LOG_ALL}"
  exit 1
}
trap killed SIGTERM

STARTTIME=$(date +%s)

function cleanup {
  ENDTIME=$(date +%s)
  echo "Runtime: $((${ENDTIME} - ${STARTTIME})) seconds"
  rm -rf "${RAM_TMP_DIR}"
  echo "  Removed  ${RAM_TMP_DIR}"
  echo ""
}
trap cleanup EXIT


echo "  Created  ${RAM_TMP_DIR}: application build dir"
echo ""

# build settings file
touch "${SETTINGS}"
echo "outputPath = \"${RAM_TMP_DIR}\"" >> "${SETTINGS}"
echo "l4file = \"${L4}\"" >> "${SETTINGS}"
echo "binary = \"${TMP_BIN}\"" >> "${SETTINGS}"

echo "Run generator:"
cd ${TESTING_DIR}  # there is no possibility to explicitly set the working directory of the jvm... (changing property user.dir does not work in all situations)
srun java -cp "${COMPILER}" ${MAIN} "${SETTINGS}" "${KNOWLEDGE}"
    if [[ $? -ne 0 ]]; then
      echo ""
      echo "ERROR: generator return code unequal to 0."
      echo ""
      touch ${ERROR_MARKER}
      echo "${LINK}" >> "${LOG_ALL}"
      exit 1
    fi
echo ""
echo "Call make:"
srun make -C "${RAM_TMP_DIR}" -j ${SLURM_CPUS_ON_NODE}
    if [[ $? -ne 0 ]]; then
      echo ""
      echo "ERROR: make return code unequal to 0."
      echo ""
      touch ${ERROR_MARKER}
      echo "${LINK}" >> "${LOG_ALL}"
      exit 1
    fi
echo ""

cp "${RAM_TMP_DIR}/${TMP_BIN}" "${BIN}" # store in NFS, as testrun could be enqueued on a different machine
