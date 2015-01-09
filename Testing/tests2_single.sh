#!/bin/bash
#SBATCH --job-name=exatest_single
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --time=15
#SBATCH --signal=INT@5


TESTING_DIR=${1}
COMPILER=${2}
MAIN=${3}
BIN=${4}
KNOWLEDGE=${5}
ERROR_MARKER=${6}

echo "Generate and compile on machine ${SLURM_JOB_NODELIST}."
rm -f ${ERROR_MARKER} # remove error marker from old job run if we were requeued

RAM_TMP_DIR="$(mktemp --tmpdir=/run/shm -d)" || {
    echo "ERROR: Failed to create temporary directory."
    touch ${ERROR_MARKER}
    exit 1
  }
OUTPUT="${RAM_TMP_DIR}/command_output.txt"
SETTINGS="${RAM_TMP_DIR}/settings.txt"
L4="${RAM_TMP_DIR}/l4.exa"
TMP_BIN="exastencils"


function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch ${ERROR_MARKER}
  exit 1
}
trap killed SIGTERM

function cleanup {
  rm -rf "${RAM_TMP_DIR}"
  echo "Removed  ${RAM_TMP_DIR}"
}
trap cleanup EXIT


echo "Created  ${RAM_TMP_DIR}: application build dir"

# build settings file
touch "${SETTINGS}"
echo "outputPath = \"${RAM_TMP_DIR}\"" >> "${SETTINGS}"
echo "l4file = \"${L4}\"" >> "${SETTINGS}"
echo "binary = \"${TMP_BIN}\"" >> "${SETTINGS}"

touch "${OUTPUT}"
cd ${TESTING_DIR}  # there is no possibility to explicitly set the working directory of the jvm... (changing property user.dir does not work in all situations)
srun java -cp "${COMPILER}" ${MAIN} "${SETTINGS}" "${KNOWLEDGE}" > "${OUTPUT}"
    if [[ $? -ne 0 ]]; then
      echo "ERROR: generator error."
      touch ${ERROR_MARKER}
      exit 1
    fi
srun make -C "${RAM_TMP_DIR}" -j ${SLURM_CPUS_ON_NODE} > "${OUTPUT}"
    if [[ $? -ne 0 ]]; then
      echo "ERROR: target compiler error."
      touch ${ERROR_MARKER}
      exit 1
    fi

cp "${RAM_TMP_DIR}/${TMP_BIN}" "${BIN}" # store in NFS, as testrun could be enqueued on a different machine
echo ""
