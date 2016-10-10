#!/bin/sh

# Shell wrapper to execute ScalaExaStencil.
# Author: Georg Altmann <georg.altmann@fau.de>

SCALAEXASTENCIL_HOME=$(dirname "$0")
SCALAEXASTENCIL_HOME=$(readlink -f "$SCALAEXASTENCIL_HOME/..")

if ! . "$SCALAEXASTENCIL_HOME/util/lib.sh" ; then
	echo "Failed to source utility shell code."
fi

exec "$JAVA_BIN" -cp "$TOOL_CP" Main $@
