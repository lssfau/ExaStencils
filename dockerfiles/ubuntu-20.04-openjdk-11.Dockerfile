FROM nvidia/cuda:11.4.1-devel-ubuntu20.04

ENV DEBIAN_FRONTEND="noninteractive" TZ="Europe/Berlin"

ARG SBT_VERSION=1.3.2
ARG LIKWID_VERSION=5.2.0

# basic stuff
RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-11-jdk python3 python3-pip openmpi-bin openmpi-common libopenmpi-dev libopenmpi3 ant mlocate time wget gfortran libjpeg-dev libpng-dev && \
  python3 -m pip install sympy numpy influxdb GitPython

# sbt
RUN \
  (echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list) && \
  (echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list) && \
  (curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add) && \
  apt-get update && \
  apt-get install -y sbt=$SBT_VERSION && \
  sbt -Dsbt.rootdir=true sbtVersion

# likwid
RUN \
  wget http://ftp.fau.de/pub/likwid/likwid-$LIKWID_VERSION.tar.gz && \
  tar -xf likwid-$LIKWID_VERSION.tar.gz && \
  cd likwid-$LIKWID_VERSION && \
  sed -i 's/\/usr\/local/\/usr\/local\/likwid/g' config.mk && \
  make && \
  make install && \
  cd .. && \
  export PATH="/usr/local/likwid/bin/:${PATH}" && \
  export PATH="/usr/local/likwid/sbin/:${PATH}" && \
  export LIKWID_INCLUDE="-I/usr/local/likwid/include" && \
  export LIKWID_LIB="-L/usr/local/likwid/lib"
