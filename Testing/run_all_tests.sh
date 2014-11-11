#!/bin/bash
#SBATCH --job-name=exastencils_daily_tests
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=5


BASE_DIR=${1} # repo root directory
LOG=${2}

COMPILER_DIR="${BASE_DIR}/Compiler"
ANT_BUILD="${COMPILER_DIR}/build.xml"
COMPILER_JAR="${COMPILER_DIR}/compiler.jar"
TESTING_DIR="${BASE_DIR}/Testing"
TESTING_CONF="${TESTING_DIR}/test_confs.txt"
TESTING_BIN_DIR="${TESTING_DIR}/bin_testing"

FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_SUBJECT="ExaStencils TestBot Error"

TMP_DIR="$(mktemp --tmpdir=/run/shm -d)"
OUTPUT="${TMP_DIR}/command_output.txt"

TIMEOUT=1


function cleanup {
  rm -rf "${TMP_DIR}"
  echo "    Removed  ${TMP_DIR}" >> "${LOG}"
  if [[ ${TIMEOUT} -eq 1 ]]; then
    echo "=== FAILURE: Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (build compiler)." >> "${LOG}"
    echo "Automatic tests failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (build compiler)." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
  fi
}
trap cleanup EXIT

# ensure this script finishes with this function (even in case of an error) to prevent incorrect timeout error
function finish {
  TIMEOUT=0
  exit 0
}

mkdir -p "${TESTING_BIN_DIR}"
# cancel all uncompleted jobs from last testrun
first=1
for job in $(squeue -h -u ${USER} -o %i); do
  if [[ ${job} -ne ${SLURM_JOB_ID} ]]; then
    if [[ first -eq 1 ]]; then
      first=0
      echo "    Old tests from last run found. Cancel them and requeue new tests (may result in timeout errors)." >> "${LOG}"
    fi
    echo "    Cancel ID ${job}." >> "${LOG}"
    scancel ${job}
  fi
done
# remove old binaries (if some)
rm -f "${TESTING_BIN_DIR}/*"

# build generator (place class files in TMP_DIR)
echo "    Created  ${TMP_DIR}: generator build dir" >> "${LOG}"
srun ant -f "${ANT_BUILD}" -Dbuild.dir="${TMP_DIR}" -Djava.dir="/usr/lib/jvm/default-java/" -Dscala.dir="/scratch/${USER}/exastencils_tests/scala/" clean build > "${OUTPUT}" 2>&1
#srun ant -f "${ANT_BUILD}" -Dbuild.dir="${TMP_DIR}" clean build > "${OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "=== FAILED: Generator: ant build error. =" >> "${LOG}"
      echo "Automatic tests failed!  Unable to compile generator." | mail -s "${FAILURE_SUBJECT}" -A "${OUTPUT}" ${FAILURE_MAIL}
      finish
    fi

# process all jobs
while read line
do

  # stip comments
  line2=${line%%\#*}
  if [[ $line2 =~ ^[" \t"]*$ ]]; then
    continue # empty line, nothing to do...
  fi

  # extract fields
  read id main knowledge result nodes cores constraints <<< $line2

  # ${knowledge} must present and either all of ${result}, ${nodes} and ${cores} must be valid or none of them
  if [[ ! -f "${TESTING_DIR}/${knowledge}" ]] || [[ ! ( -f "${TESTING_DIR}/${result}" && ${nodes} =~ ^[0-9]+$ && ${cores} =~ ^[0-9]+$ ) && ! ( ${result} = "" && ${nodes} = "" && ${cores} = "" ) ]]; then
    echo "=== FAILED: Configuration: invalid test line:  '${line}'" >> "${LOG}"
    echo "Test failed!  Invalid configuration line:  '${line}'." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
    continue
  fi

  # configuration is fine, start a new job for it (${nodes} and ${cores} may be empty, so they must be passed at the last!)
  sbatch "${TESTING_DIR}/run_single_test.sh" "${TESTING_DIR}" "${TESTING_BIN_DIR}" ${id} "${COMPILER_JAR}" ${main} "${TESTING_DIR}/${knowledge}" "${FAILURE_MAIL}" "${LOG}" "${TESTING_DIR}/${result}" ${nodes} ${cores} "${constraints}"
done < "${TESTING_CONF}"

finish
