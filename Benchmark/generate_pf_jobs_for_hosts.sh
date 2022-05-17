#!/bin/bash -l

HOSTLIST=$(sinfo -t idle -h --partition=work -o "%n")
for HOST in ${HOSTLIST}; do
  echo "stages:"
  echo "    - pf_pipe"
  echo ""
  echo "$(<.benchmark_templates.yml)"
  echo ""
  echo "pf-gen-$HOST:"
  echo -e "    stage: pf_pipe"
  echo -e "    extends: .pfgen_template"
  echo -e "    variables:"
  echo -e "        HOSTNAME: \"$HOST\""
  echo
done