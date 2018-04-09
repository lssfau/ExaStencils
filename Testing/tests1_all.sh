#!/bin/bash
#SBATCH --job-name=et_all
#SBATCH -p anywhere
#SBATCH -A anywhere
#SBATCH --qos=norm
#SBATCH -n 1
#SBATCH -c 4
#SBATCH --exclusive
#SBATCH --time=10
#SBATCH --signal=INT@10
#SBATCH --open-mode=append


SCR_DIR=${1}
REPO_DIR=${2}
SCALA_DIR=${3}
TEMP_DIR=${4}
OUT_FILE=${5} # stdout and stderr should already be redirected to this file
OUT_FILE_URL=${6} # url to ${OUT_FILE}
PROGRESS=${7}
BRANCH=${8}
FAILURE_MAIL=${9}


# HACK: otherwise ant wouldn't find it...
JAVA_DIR="/usr/lib/jvm/default-java/"

PREV_TEMP_DIR="${TEMP_DIR}/prev"
COMPILER_JAR="${TEMP_DIR}/compiler.jar"

ANT_BUILD="${REPO_DIR}/Compiler/build.xml"
TESTING_DIR="${REPO_DIR}/Testing"
TESTING_CONF="${TESTING_DIR}/test_confs.txt"

FAILURE_MAIL_FILE="${TESTING_DIR}/failure_mails.txt"
if [[ -z ${FAILURE_MAIL} ]]; then
  if [[ -s "${FAILURE_MAIL_FILE}" ]]; then
    FAILURE_MAIL=$(cat ${FAILURE_MAIL_FILE})
  else
    FAILURE_MAIL="exastencils-dev@www.uni-passau.de"
  fi
fi

ERROR_MARKER_NAME="error"
ERROR_MARKER="${TEMP_DIR}/${ERROR_MARKER_NAME}"

LOG_DIR=$(dirname "${OUT_FILE}")


function update_progress {
  if [[ "${1}" -eq 0 ]]; then
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)  (Reload this page manually.)\n Log can be found <a href=./${BRANCH}/>here</a>.\n\n  Done!\n\n  New tests can be triggered <a href=../trigger-eg-tests.html>here</a></div></body></html>" > "${PROGRESS}"
  elif [[ "${1}" -eq 1 ]]; then
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)  (Reload this page manually.)\n Log can be found <a href=./${BRANCH}/>here</a>.\n\n$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R")</div></body></html>" > "${PROGRESS}"
  else
    echo -e "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">Branch: ${BRANCH};\n last update: $(date -R)  (Reload this page manually.)\n Log can be found <a href=./${BRANCH}/>here</a>.\n\n$(squeue -u exatest -o "%.11i %10P %25j %3t %.11M %.5D %R" | grep -v ${SLURM_JOB_ID})</div></body></html>" > "${PROGRESS}"
  fi
}

function error {
  update_progress 0
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

RAM_TMP_DIR=$(mktemp --tmpdir=/dev/shm -d || mktemp --tmpdir=/tmp -d) || {
    echo "ERROR: Failed to create temporary directory."
    error
  }
if [[ ! ${RAM_TMP_DIR} =~ ^/dev/shm/* ]]; then
  echo "Problems with /dev/shm on machine ${SLURM_JOB_NODELIST} in job ${SLURM_JOB_NAME}:${SLURM_JOB_ID}." | mail -s "ExaTest /dev/shm" "kronast@fim.uni-passau.de"
fi

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


update_progress 1

echo "-----------------------------------------------------------------------------------------------"
echo "Running main test script on machine ${SLURM_JOB_NODELIST} (${SLURM_JOB_NAME}:${SLURM_JOB_ID})."
echo "  Created  ${RAM_TMP_DIR}: generator build dir"


# cancel all uncompleted jobs from last testrun
OLD_JOBS="$(squeue -h -u exatest -o %i | grep -v ${SLURM_JOB_ID})"
if [[ -n "${OLD_JOBS}" ]]; then
  echo "Old tests from last run found. Cancel them and requeue new tests."
  for job in ${OLD_JOBS}; do
    scancel ${job}
    echo "Old job ${job} canceled."
  done
fi
# remove old files (if some) and backup existing
if [[ -e "${COMPILER_JAR}" ]]; then
  mkdir -p "${PREV_TEMP_DIR}"
  rm -rf "${PREV_TEMP_DIR}"/*
  find "${TEMP_DIR}" -maxdepth 1 -mindepth 1 ! -samefile "${PREV_TEMP_DIR}" -exec mv '{}' "${PREV_TEMP_DIR}" \;
else
  find "${TEMP_DIR}" -maxdepth 1 -mindepth 1 ! -samefile "${PREV_TEMP_DIR}" -exec rm -rf '{}' \;
fi
TESTS_DIR="${TEMP_DIR}/tests"
mkdir "${TESTS_DIR}"

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
echo ""
echo "-----------------------------------------------------------------------------------------------"
echo "Parse configuration file and enqueue subjobs:"
echo ""

declare -a TMP_ARRAY
DEP_SIDS=""
i=0

# read config and enqueue compilation jobs
while read line
do

  # strip comments
  line2=${line%%\#*}
  if [[ $line2 =~ ^[" \t"]*$ ]]; then
    continue # empty line, nothing to do...
  fi

  # extract fields
  read id main knowledge exafiles result nodes cores constraints <<< $line2

  # ${knowledge} must present and either all of ${result}, ${nodes} and ${cores} must be valid or not given at all
  if [[ ! -f "${TESTING_DIR}/${knowledge}" ]] || [[ ! ( -f "${TESTING_DIR}/${result}" && ${nodes} =~ ^[0-9]+$ && ${cores} =~ ^[0-9]+$ ) && ! ( -z ${result} && -z ${nodes} && -z ${cores} ) ]]; then
    echo "ERROR:  Invalid configuration line:  '${line}'"
    touch "${ERROR_MARKER}"
    continue
  fi

  TEST_LOG_REL="${id}.html"
  TEST_LOG="${LOG_DIR}/${TEST_LOG_REL}"
  TEST_ERROR_MARKER="${TEST_LOG}.${ERROR_MARKER_NAME}"
  TEST_DIR="${TESTS_DIR}/${id}"
  TEST_BIN="exastencils.exe"

  if [[ "${exafiles}" = "*" ]]; then
    exafiles=""
  fi
  # test if ${exafiles} exist and create absolute paths
  IFS=';' read -a exaf <<< $exafiles
  exafiles=""
  for f in "${exaf[@]}"; do
    abs="${TESTING_DIR}/${f}"
    if [[ -f "${abs}" ]]; then
      case ${abs} in
        *.exa1) ;;
        *.exa2) ;;
        *.exa3) ;;
        *.exa4) ;;
        *)
          echo "<span style=\"color: #FF8000\">WARNING: skip input file '${f}' (unknown file extension)</span>"
          continue # do not add it to ${exafiles}
          ;;
      esac
      exafiles="${exafiles}${abs};" # place delimiter at the end to ensure the resulting string does not start with it
    else
      echo "ERROR:  Invalid configuration line:  '${line}': ExaSlang file '${f}' not found"
      touch "${ERROR_MARKER}"
      continue 2 # continue outer while loop
    fi
  done

  COMPILE_CONSTR=""
  if [[ ${constraints} =~ GPU ]] || [[ ${constraints} = "E5" ]]; then
    PLATFORM="chimaira.platform"
    COMPILE_CONSTR="-A cl -p chimaira -c 20" # HACK: the cuda compiler is not installed on all machines; use more compile threads for CUDA code
  elif [[ ${constraints} = "AVX2" ]]; then
    PLATFORM="anyavx2.platform"
  elif [[ ${constraints} = "AVX" ]]; then
    PLATFORM="anyavx.platform"
  else
    PLATFORM="random.platform"
  fi

  echo "<html><head><meta charset=\"utf-8\"></head><body><div style=\"white-space: pre-wrap; font-family:monospace;\">" > "${TEST_LOG}"
  echo "Test ID:  ${id}" >> "${TEST_LOG}"
  echo "<a href=./>Back to overview.</a>" >> "${TEST_LOG}"

  echo "Enqueue generation and compilation job, as well as execution job for id  ${id}."
  # configuration is fine, start a new job for it
  OUT=$(unset SLURM_JOB_NAME; sbatch --job-name="etg_${id}" -o ${TEST_LOG} -e ${TEST_LOG} ${COMPILE_CONSTR} "--dependency=afterok:${SLURM_JOB_ID}" "${SCR_DIR}/tests2_single.sh" "${TESTING_DIR}" "${COMPILER_JAR}" ${main} "${TEST_DIR}" "${TEST_BIN}" "${TESTING_DIR}/${knowledge}" "${exafiles}" "${TESTING_DIR}/Platform/${PLATFORM}" "${TEST_ERROR_MARKER}" "${OUT_FILE}" "<a href=./${TEST_LOG_REL}>${id}</a>" "${PROGRESS}" "${BRANCH}")
  if [[ $? -eq 0 ]]; then
    SID=${OUT#Submitted batch job }
    DEP_SIDS="${DEP_SIDS}:${SID}"
  else
    touch "${ERROR_MARKER}"
  fi
  echo "  ${OUT}"
  if [[ -n ${cores} ]]; then
    TEST_DEP="--dependency=afterok:${SID}"

    ACC="anywhere"
    PART="anywhere"
    CONSTR_PARAM="--constraint=${constraints}"
    if [[ ${constraints} =~ mem[0-9]* ]]; then
      CONSTR_PARAM="--mem-per-cpu=${constraints#mem} --constraint="
      constraints=""
    fi
    if [[ ${nodes} -gt 1 ]]; then # use the clusters only if MPI is required
      if [[ -n ${constraints} ]]; then
        CONSTR_PARAM="${CONSTR_PARAM}|"
      fi
      CONSTR_PARAM="${CONSTR_PARAM}chimaira|zmiy|zeus" # cayman still suffers from an MPI bug...
    fi
#    if [[ ${constraints} = "E5" ]]; then # HACK to ensure jobs are executed even if the cluster is in use
#      ACC="cl"
#      PART="chimaira"
#      CONSTR_PARAM=""
#    fi
    if [[ ${constraints} =~ GPU ]]; then
      ACC="cl"
      PART="chimaira"
      CONSTR_PARAM="--gres=gpu:1"
    fi
    OUT=$(unset SLURM_JOB_NAME; sbatch --job-name="etr_${id}" -o ${TEST_LOG} -e ${TEST_LOG} -A ${ACC} -p ${PART} -n ${nodes} -c ${cores} ${TEST_DEP} ${CONSTR_PARAM} "${SCR_DIR}/tests3_generated.sh" "${TEST_DIR}/${TEST_BIN}" "${TESTING_DIR}/${result}" "${TEMP_DIR}" "${TEST_ERROR_MARKER}" "${OUT_FILE}" "<a href=./${TEST_LOG_REL}>${id}</a>" "${PROGRESS}" "${BRANCH}")
    if [[ $? -eq 0 ]]; then
      SID=${OUT#Submitted batch job }
      DEP_SIDS="${DEP_SIDS}:${SID}"
    else
      touch "${ERROR_MARKER}"
    fi
    echo "  ${OUT}"
  else
    echo "  No execution required."
    echo "Test OK (must not be executed)" >> "${TEST_LOG}"
  fi
  echo "  <a href=./${TEST_LOG_REL}>Log...</a>"
done < "${TESTING_CONF}"

echo ""
echo "Collect separate logs after all other jobs are finished:"
LOG_DEPS="--dependency=afterany${DEP_SIDS}"
(unset SLURM_JOB_NAME; sbatch -o "${OUT_FILE}" -e "${OUT_FILE}" ${LOG_DEPS} "${SCR_DIR}/tests4_logs.sh" "${FAILURE_MAIL}" "${OUT_FILE}" "${OUT_FILE_URL}" "${ERROR_MARKER_NAME}" "${ERROR_MARKER}" "${LOG_DIR}" "${PROGRESS}" "${BRANCH}" "${TESTS_DIR}")
echo ""

update_progress 2
