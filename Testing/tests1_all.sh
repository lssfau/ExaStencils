#!/bin/bash
#SBATCH --job-name=exatest_all
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH -o /dev/null
#SBATCH -e /dev/null
#SBATCH --time=5


REPO_DIR=${1}
TEMP_DIR=${2}
LOG=${3}

# HACK: otherwise ant wouldn't find them...
JAVA_DIR="/usr/lib/jvm/default-java/"
SCALA_DIR="/scratch/${USER}/exastencils_tests/scala/"

COMPILER_JAR="${TEMP_DIR}/compiler.jar"

ANT_BUILD="${REPO_DIR}/Compiler/build.xml"
TESTING_DIR="${REPO_DIR}/Testing"
TESTING_CONF="${TESTING_DIR}/test_confs.txt"

FAILURE_MAIL="exastencils-dev@www.uni-passau.de"
FAILURE_MAIL_SUBJECT="TestBot Error"

RAM_TMP_DIR=$(mktemp --tmpdir=/run/shm -d) || {
    echo "=== FAILURE: Failed to create temporary directory on machine $(hostname) in ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (build compiler)." >> "${LOG}"
    echo "Automatic tests failed!  Unable to create temporary directory in ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (build compiler)." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
    exit 0
  }
ANT_OUTPUT="${RAM_TMP_DIR}/ant_output.txt"


function timeout {
  echo "=== FAILURE: Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (build compiler)." >> "${LOG}"
  echo "Automatic tests failed!  Timeout in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} (build compiler)." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
}
trap timeout SIGINT

function cleanup {
  rm -rf "${RAM_TMP_DIR}"
  echo "    Removed  ${RAM_TMP_DIR}" >> "${LOG}"
}
trap cleanup EXIT


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
# remove old binaries (if some); do not quote the *, as this prevents file expansion...
rm -f "${TEMP_DIR}"/*

# build generator (place class files in RAM_TMP_DIR)
echo "    Created  ${RAM_TMP_DIR}: generator build dir" >> "${LOG}"
srun ant -f "${ANT_BUILD}" -Dbuild.dir="${RAM_TMP_DIR}/build" -Dcompiler.jar="${COMPILER_JAR}" -Djava.dir="${JAVA_DIR}" -Dscala.dir="${SCALA_DIR}" clean build > "${ANT_OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "=== FAILED: Generator: ant build error. =" >> "${LOG}"
      echo "Automatic tests failed!  Unable to compile generator." | mail -s "${FAILURE_MAIL_SUBJECT}" -A "${ANT_OUTPUT}" ${FAILURE_MAIL}
      exit 0
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
    echo "Test failed!  Invalid configuration line:  '${line}'." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
    continue
  fi

  # configuration is fine, start a new job for it (${nodes} and ${cores} may be empty, so they must be passed at the last!)
  sbatch "${TESTING_DIR}/tests2_single.sh" "${TESTING_DIR}" "${TEMP_DIR}" ${id} "${COMPILER_JAR}" ${main} "${TESTING_DIR}/${knowledge}" ${FAILURE_MAIL} "${FAILURE_MAIL_SUBJECT}" "${LOG}" "${TESTING_DIR}/${result}" ${nodes} ${cores} "${constraints}"
      if [[ $? -ne 0 ]]; then
        echo "=== FAILED: Unable to enqueue job for id ${id}." >> "${LOG}"
        echo "Test failed!  Unable to enqueue job for id ${id}." | mail -s "${FAILURE_MAIL_SUBJECT}" ${FAILURE_MAIL}
      fi
done < "${TESTING_CONF}"
