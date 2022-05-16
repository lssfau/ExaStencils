#!/bin/bash -l

HOSTLIST=$(sinfo -h --partition=work -o "%n")
for HOST in ${HOSTLIST}; do
  echo "stages:"
  echo "    - pf_gen"
  echo ""
  echo "$(<Benchmark/.benchmark_templates.yml)"
  echo ""
  echo "pf-gen-$HOST:"
  echo -e "    stage: pf_gen"
  echo -e "    extends: .pfgen_template"
  echo -e "    variables:"
  echo -e "        HOSTNAME: \"$HOST\""
  echo
done