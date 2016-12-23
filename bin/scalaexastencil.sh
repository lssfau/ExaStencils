#!/bin/sh

# Shell wrapper to execute ScalaExaStencil.
# Author: Georg Altmann <georg.altmann@fau.de>

SCALAEXASTENCIL_HOME=$(dirname "$0")
SCALAEXASTENCIL_HOME=$(readlink -f "$SCALAEXASTENCIL_HOME/..")

. "$SCALAEXASTENCIL_HOME/util/lib.sh"

exec "$JAVA_BIN" -cp "$TOOL_CP" Main $@
