#!/bin/sh

# Generate scaladoc from ScalaExaStencil sources.
# author: Georg Altmann <georg.altmann@fau.de>
#
# usage:
# Execute in ScalaExaStencil source root, e.g. "sh util/gen-scaladoc.sh".
# scaladoc HTML files are generated in "doc".

# Output directory for scaladoc
DOC_DIR="doc"

cleanup() {
  rm -f .scaladocargs
}

# no cleanup!
die() {
  echo $1
  exit 1
}

handle_signal() {
  cleanup
  exit 2
}

### LOCATE JDK ############################################################### 
# The following is adapted from jetbrains idea.sh

UNAME=`which uname`
GREP=`which egrep`
GREP_OPTIONS=""
CUT=`which cut`
READLINK=`which readlink`
XARGS=`which xargs`
DIRNAME=`which dirname`
MKTEMP=`which mktemp`
RM=`which rm`
CAT=`which cat`
TR=`which tr`

if [ -z "$UNAME" -o -z "$GREP" -o -z "$CUT" -o -z "$MKTEMP" -o -z "$RM" -o -z "$CAT" -o -z "$TR" ]; then
  die "Required tools are missing - check beginning of \"$0\" file for details."
fi

OS_TYPE=`"$UNAME" -s`
if [ -n "$JDK_HOME" -a -x "$JDK_HOME/bin/java" ]; then
  JDK="$JDK_HOME"
elif [ -n "$JAVA_HOME" -a -x "$JAVA_HOME/bin/java" ]; then
  JDK="$JAVA_HOME"
else
  JAVA_BIN_PATH=`which java`
  if [ -n "$JAVA_BIN_PATH" ]; then
    if [ "$OS_TYPE" = "FreeBSD" -o "$OS_TYPE" = "MidnightBSD" ]; then
      JAVA_LOCATION=`JAVAVM_DRYRUN=yes java | "$GREP" '^JAVA_HOME' | "$CUT" -c11-`
      if [ -x "$JAVA_LOCATION/bin/java" ]; then
        JDK="$JAVA_LOCATION"
      fi
    elif [ "$OS_TYPE" = "SunOS" ]; then
      JAVA_LOCATION="/usr/jdk/latest"
      if [ -x "$JAVA_LOCATION/bin/java" ]; then
        JDK="$JAVA_LOCATION"
      fi
    elif [ "$OS_TYPE" = "Darwin" ]; then
      JAVA_LOCATION=`/usr/libexec/java_home`
      if [ -x "$JAVA_LOCATION/bin/java" ]; then
        JDK="$JAVA_LOCATION"
      fi
    fi

    if [ -z "$JDK" -a -x "$READLINK" -a -x "$XARGS" -a -x "$DIRNAME" ]; then
      JAVA_LOCATION=`"$READLINK" -f "$JAVA_BIN_PATH"`
      case "$JAVA_LOCATION" in
        */jre/bin/java)
          JAVA_LOCATION=`echo "$JAVA_LOCATION" | "$XARGS" "$DIRNAME" | "$XARGS" "$DIRNAME" | "$XARGS" "$DIRNAME"`
          if [ ! -d "$JAVA_LOCATION/bin" ]; then
            JAVA_LOCATION="$JAVA_LOCATION/jre"
          fi
          ;;
        *)
          JAVA_LOCATION=`echo "$JAVA_LOCATION" | "$XARGS" "$DIRNAME" | "$XARGS" "$DIRNAME"`
          ;;
      esac
      if [ -x "$JAVA_LOCATION/bin/java" ]; then
        JDK="$JAVA_LOCATION"
      fi
    fi
  fi
fi

JAVA_BIN="$JDK/bin/java"
if [ ! -x "$JAVA_BIN" ]; then
  JAVA_BIN="$JDK/jre/bin/java"
fi

if [ -z "$JDK" ] || [ ! -x "$JAVA_BIN" ]; then
  echo "No JDK found. Please validate either JDK_HOME or JAVA_HOME environment variable points to valid JDK installation or java binary is in PATH."
  exit 1
fi
### END LOCATE JDK ########################################################### 

### LOCATE SCALA #############################################################

if [ -n "$SCALA_HOME" ] ; then
  SCALA_BIN=$(readlink -f "$SCALA_HOME/bin/scala")
  if [ ! -x "$SCALA_BIN" ] ; then
    die "Environment variable SCALA_HOME does not point to a valid scala installation."
  fi
  SCALA_LIB="$SCALA_HOME/lib"
  SCALA_LIB_JAR="$SCALA_LIB/scala-library.jar"
  if [ ! -e "$SCALA_LIB_JAR" ] ; then
    die "Environment variable SCALA_HOME does not point to a valid scala installation."
  fi
  SCALADOC_BIN="$SCALA_HOME/bin/scaladoc"
else
  # attempt so find scala installation via scala bin
  if ! SCALA_BIN=$(which scala) ; then
    die "Scala binary not found / not in PATH."
  fi
  SCALA_BIN=$(readlink -f "$SCALA_BIN")

  SCALA_HOME=$(dirname "$SCALA_BIN")
  SCALA_HOME="$SCALA_HOME/.."
  SCALA_HOME=$(readlink -f "$SCALA_HOME")

  SCALA_LIB="$SCALA_HOME/lib"

  SCALA_LIB_JAR="$SCALA_LIB/scala-library.jar"
  if [ ! -e "$SCALA_LIB_JAR" ] ; then
    echo "Failed to resolve scala lib directory from scala binary:"
    echo "  scala bin: $SCALA_BIN"
    echo "  scala lib: $SCALA_LIB"
    exit 1
  fi

  SCALADOC_BIN="$SCALA_HOME/bin/scaladoc"
fi

if [ ! -e "$SCALADOC_BIN" ] ; then 
  die "scaladoc binary not found. Check PATH or SCALA_HOME environment variables."
fi

### END LOCATE SCALA #########################################################


# setup class path
SCALADOC_CP="Compiler/bin"
SCALADOC_CP="$SCALADOC_CP:Compiler/lib/*"
SCALADOC_CP="$SCALADOC_CP:Compiler/lib"
SCALADOC_CP="$SCALADOC_CP:CompilerMacros/bin"

SCALADOC_CP="$SCALADOC_CP:$SCALA_LIB/*"
SCALADOC_CP="$SCALADOC_CP:$JDK/lib/*"

if false ; then
  echo "SCALADOC_CP: $SCALADOC_CP"
  exit 0
fi

# sanity check
if [ ! -d Compiler/src/exastencils ] ||
  [ ! -d CompilerMacros/src/exastencils ] ; then
die "ScalaExaStencil sources not found. Is the current directory the ScalaExaStencil sources root directory?"
fi

if [ ! -d "$DOC_DIR" ] ; then mkdir "$DOC_DIR" || die "Failed to make scaladoc output directory: $DOC_DIR" ; fi
rm -f .scaladocargs

trap handle_signal 1 2 3 6 15

echo "-classpath \"$SCALADOC_CP\"" >> .scaladocargs
echo "-d \"$DOC_DIR\" " >> .scaladocargs
find Compiler/src/exastencils \
  CompilerMacros/src/exastencils \
  -name '*.scala' >> .scaladocargs

"$SCALADOC_BIN" @.scaladocargs
cleanup
exit 0
