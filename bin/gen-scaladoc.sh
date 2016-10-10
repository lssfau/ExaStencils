#!/bin/sh

# Generate scaladoc from ScalaExaStencil sources.
# Author: Georg Altmann <georg.altmann@fau.de>
#
# usage:
# Execute in ScalaExaStencil source root, e.g. "sh bin/gen-scaladoc.sh".
# scaladoc HTML files are generated in "doc".

# Output directory for scaladoc
DOC_DIR="doc"

cleanup() {
  rm -f .scaladocargs
}

handle_signal() {
  cleanup
  exit 2
}

SCALAEXASTENCIL_HOME=$(dirname "$0")
SCALAEXASTENCIL_HOME=$(readlink -f "$SCALAEXASTENCIL_HOME/..")

. "$SCALAEXASTENCIL_HOME/util/lib.sh"

# sanity check
if [ ! -d Compiler/src/exastencils ] ||
  [ ! -d CompilerMacros/src/exastencils ] ; then
die "ScalaExaStencil sources not found. Is the current directory the ScalaExaStencil sources root directory?"
fi

if [ ! -d "$DOC_DIR" ] ; then mkdir "$DOC_DIR" || die "Failed to make scaladoc output directory: $DOC_DIR" ; fi
rm -f .scaladocargs

trap handle_signal 1 2 3 6 15

echo "-classpath \"$TOOL_CP\"" >> .scaladocargs
echo "-d \"$DOC_DIR\" " >> .scaladocargs
find Compiler/src/exastencils \
  CompilerMacros/src/exastencils \
  -name '*.scala' >> .scaladocargs

"$SCALADOC_BIN" @.scaladocargs
cleanup
exit 0
