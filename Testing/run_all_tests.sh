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

FAILURE_MAIL="kronast@fim.uni-passau.de"
FAILURE_SUBJECT="ExaStencils TestBot Error"

TMP_DIR="$(mktemp --tmpdir=/run/shm -d)"
OUTPUT="${TMP_DIR}/command_output.txt"

TIMEOUT=1


function finish {
  rm -rf "${TMP_DIR}"
  echo "    Removed  ${TMP_DIR}" >> "${LOG}"
  if [[ ${TIMEOUT} -eq 1 ]]; then
    echo "=== FAILURE: Timeout when calling ${BASH_SOURCE} (build compiler)" >> "${LOG}"
	echo "Automatic tests failed!  Timeout when calling ${BASH_SOURCE} (build compiler)" | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
  fi
}
trap finish EXIT

# build generator (place class files in TMP_DIR)
echo "    Created  ${TMP_DIR}: generator build dir" >> "${LOG}"
srun ant -f "${ANT_BUILD}" -Dbuild.dir="${TMP_DIR}" -Djava.dir="/usr/lib/jvm/default-java/" -Dscala.dir="/scratch/kronast/scala/" clean build > "${OUTPUT}" 2>&1
#srun ant -f "${ANT_BUILD}" -Dbuild.dir="${TMP_DIR}" clean build > "${OUTPUT}" 2>&1
    if [[ $? -ne 0 ]]; then
      echo "=== FAILED: Generator: ant build error. =" >> "${LOG}"
      echo "Automatic tests failed!  Unable to compile generator." | mail -s "${FAILURE_SUBJECT}" -A "${OUTPUT}" ${FAILURE_MAIL}
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
  read id main knowledge result nodes cores <<< $line2

  # ${knowledge} must present and either all of ${result}, ${nodes} and ${cores} must be valid or none of them
  if [[ ! -f "${TESTING_DIR}/${knowledge}" ]] || [[ ! ( -f "${TESTING_DIR}/${result}" && ${nodes} =~ ^[0-9]+$ && ${cores} =~ ^[0-9]+$ ) && ! ( ${result} = "" && ${nodes} = "" && ${cores} = "" ) ]]; then
    echo "=== FAILED: Configuration: invalid test line:  '${line}'" >> "${LOG}"
    echo "Test failed!  Invalid configuration line:  '${line}'." | mail -s "${FAILURE_SUBJECT}" ${FAILURE_MAIL}
    continue
  fi

  # configuration is fine, start a new job for it (${nodes} and ${cores} may be empty, so they must be passed at the last!)
  sbatch "${TESTING_DIR}/run_single_test.sh" "${TESTING_DIR}" ${id} "${COMPILER_JAR}" ${main} "${TESTING_DIR}/${knowledge}" "${FAILURE_MAIL}" "${LOG}" "${TESTING_DIR}/${result}" ${nodes} ${cores}
done < "${TESTING_CONF}"

TIMEOUT=0
