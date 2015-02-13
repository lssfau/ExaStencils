#!/bin/bash
#SBATCH --job-name=et_all
#SBATCH -p idle
#SBATCH -A idle
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --hint=nomultithread
#SBATCH --cpu_bind=cores
#SBATCH --time=5
#SBATCH --signal=INT@5
#SBATCH --open-mode=append


REPO_DIR=${1}
SCALA_DIR=${2}
TEMP_DIR=${3}
OUT_FILE=${4} # stdout and stderr should already be redirected to this file
OUT_FILE_URL=${5} # url to ${OUT_FILE}
PROGRESS=${6}


# HACK: otherwise ant wouldn't find it...
JAVA_DIR="/usr/lib/jvm/default-java/"

COMPILER_JAR="${TEMP_DIR}/compiler.jar"

ANT_BUILD="${REPO_DIR}/Compiler/build.xml"
TESTING_DIR="${REPO_DIR}/Testing"
TESTING_CONF="${TESTING_DIR}/test_confs.txt"

FAILURE_MAIL="exastencils-dev@www.uni-passau.de"
TECH_FAILURE_MAIL="kronast@fim.uni-passau.de"

ERROR_MARKER_NAME="error"
ERROR_MARKER="${TEMP_DIR}/${ERROR_MARKER_NAME}"

LOG_DIR=$(dirname "${OUT_FILE}")


function error {
  echo "Automatic tests failed!  See log for details: ${OUT_FILE_URL}" | mail -s "TestBot Error" ${FAILURE_MAIL}
  exit 1
}

function killed {
  echo "ERROR? Job ${SLURM_JOB_NAME}:${SLURM_JOB_ID} killed; possible reasons: timeout, manually canceled, user login (job is then requeued)."
  touch "${ERROR_MARKER}"
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
  echo ""
  echo "Failed tests:"
}
trap cleanup EXIT


echo "<html><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.5D %R")</pre></body></html>" > "${PROGRESS}"

echo "-----------------------------------------------------------------------------------------------"
echo "Running main test script on machine ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."

RAM_TMP_DIR=$(mktemp --tmpdir=/run/shm -d) || {
    echo "ERROR: Failed to create temporary directory."
    error
  }
echo "  Created  ${RAM_TMP_DIR}: generator build dir"


# cancel all uncompleted jobs from last testrun
first=1
for job in $(squeue -h -u exatest -o %i); do
  if [[ ${job} -ne ${SLURM_JOB_ID} ]]; then
    if [[ first -eq 1 ]]; then
      first=0
      echo "Old tests from last run found. Cancel them and requeue new tests."
      echo "Old tests from last run found. Cancel them and requeue new tests." | mail -s "TestBot jobs too old" ${TECH_FAILURE_MAIL}
    fi
    scancel ${job}
	echo "Old job ${job} canceled."
  fi
done
# remove old files (if some)
rm -rf "${TEMP_DIR}"/*
BIN_DIR="${TEMP_DIR}/bin"
mkdir "${BIN_DIR}"

# build generator (place class files in RAM_TMP_DIR)
echo ""
echo "Running ant:"
srun ant -f "${ANT_BUILD}" -Dbuild.dir="${RAM_TMP_DIR}/build" -Dcompiler.jar="${COMPILER_JAR}" -Djava.dir="${JAVA_DIR}" -Dscala.dir="${SCALA_DIR}" clean build
    if [[ $? -ne 0 ]]; then
      echo "ERROR: ant build error."
      echo ""
      error
    fi
echo ""

echo "Parse configuration file and enqueue subjobs:"
echo ""

declare -a TMP_ARRAY
DEP_SIDS=""
i=0

# read config and enqueue compilation jobs
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
    echo "ERROR:  Invalid configuration line:  '${line}'"
    touch "${ERROR_MARKER}"
    continue
  fi

  TEST_LOG_REL="${id}.html"
  TEST_LOG="${LOG_DIR}/${TEST_LOG_REL}"
  TEST_ERROR_MARKER="${TEST_LOG}.${ERROR_MARKER_NAME}"
  TEST_BIN="${BIN_DIR}/${id}"

  echo "<html><body><pre>" > "${TEST_LOG}"
  echo "Test ID:  ${id}" >> "${TEST_LOG}"

  echo "Enqueue generation and compilation job for id  ${id}."
  # configuration is fine, start a new job for it
  OUT=$(sbatch --job-name="etg_${id}" -o ${TEST_LOG} -e ${TEST_LOG} "--dependency=afterok:${SLURM_JOB_ID}" "${TESTING_DIR}/tests2_single.sh" "${TESTING_DIR}" "${COMPILER_JAR}" ${main} "${TEST_BIN}" "${TESTING_DIR}/${knowledge}" "${TEST_ERROR_MARKER}" "${OUT_FILE}" "<a href=./${TEST_LOG_REL}>${id}</a>" "${PROGRESS}")
  if [[ $? -eq 0 ]]; then
    SID=${OUT#Submitted batch job }
    DEP_SIDS="${DEP_SIDS}:${SID}"
  else
    touch "${ERROR_MARKER}"
  fi
  echo "${OUT}"
  echo "<a href=./${TEST_LOG_REL}>Log...</a>"

  if [[ -n ${cores} ]]; then
    TMP_ARRAY[i+0]=${id}
    TMP_ARRAY[i+1]=${nodes}
    TMP_ARRAY[i+2]=${cores}
    TMP_ARRAY[i+3]=${constraints}
    TMP_ARRAY[i+4]=${TEST_BIN}
    TMP_ARRAY[i+5]=${result}
    TMP_ARRAY[i+6]=${TEST_LOG}
    TMP_ARRAY[i+7]=${TEST_ERROR_MARKER}
    TMP_ARRAY[i+8]=${SID}
    i=$((i+9))
  else
    echo "Test OK (must not be executed)" >> "${TEST_LOG}"
  fi
done < "${TESTING_CONF}"

echo ""
echo "Enqueue execution jobs:"
echo ""
COMP_DEPS="${DEP_SIDS}"

# enqueue execution jobs
for ((i=0;i<${#TMP_ARRAY[@]};i+=9)); do

  id=${TMP_ARRAY[i+0]}
  nodes=${TMP_ARRAY[i+1]}
  cores=${TMP_ARRAY[i+2]}
  constraints=${TMP_ARRAY[i+3]}
  TEST_BIN=${TMP_ARRAY[i+4]}
  result=${TMP_ARRAY[i+5]}
  TEST_LOG=${TMP_ARRAY[i+6]}
  TEST_ERROR_MARKER=${TMP_ARRAY[i+7]}
  SID_GEN=${TMP_ARRAY[i+8]}

  TEST_DEP="--dependency=afterok:${SID_GEN},afterany${COMP_DEPS}"

  ACC="idle"
  PART="idle"
  CONSTR_PARAM="--constraint=${constraints}"
  if [[ $(( ${nodes} * ${cores} )) -gt 30 ]] || [[ ${constraints} = "E5" ]]; then # HACK to ensure jobs are executed even if the cluster is in use
    ACC="cl"
    PART="chimaira"
    CONSTR_PARAM=""
  fi
  echo "Enqueue execution job for id  ${id}."
  OUT=$(sbatch --job-name="etr_${id}" -o ${TEST_LOG} -e ${TEST_LOG} -A ${ACC} -p ${PART} -n ${nodes} -c ${cores} ${TEST_DEP} ${CONSTR_PARAM} "${TESTING_DIR}/tests3_generated.sh" "${TEST_BIN}" "${TESTING_DIR}/${result}" "${TEST_ERROR_MARKER}" "${OUT_FILE}" "<a href=./${TEST_LOG_REL}>${id}</a>" "${PROGRESS}")
  if [[ $? -eq 0 ]]; then
    SID=${OUT#Submitted batch job }
    DEP_SIDS="${DEP_SIDS}:${SID}"
  else
    touch "${ERROR_MARKER}"
  fi
  echo "${OUT}"
  echo "<a href=./${TEST_LOG_REL}>Log...</a>"
done

echo ""
echo "Collect separate logs after all other jobs are finished:"
LOG_DEPS="--dependency=afterany${DEP_SIDS}"
(sbatch -o "${OUT_FILE}" -e "${OUT_FILE}" ${LOG_DEPS} "${TESTING_DIR}/tests4_logs.sh" "${FAILURE_MAIL}" "${OUT_FILE}" "${OUT_FILE_URL}" "${ERROR_MARKER_NAME}" "${ERROR_MARKER}" "${LOG_DIR}" "${PROGRESS}")
echo ""

echo "<html><body><pre>$(squeue -u exatest -o "%.11i %10P %25j %3t %.5D %R")</pre></body></html>" > "${PROGRESS}"
