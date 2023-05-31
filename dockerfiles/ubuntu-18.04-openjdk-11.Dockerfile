FROM nvidia/cuda:10.1-devel-ubuntu18.04

ARG SBT_VERSION=1.3.2

RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-11-jdk python3 python3-pip openmpi-bin openmpi-common libopenmpi-dev libopenmpi2 ant mlocate time wget gfortran && \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion && \
  python3 -m pip install sympy numpy influxdb GitPython && \
  wget http://ftp.fau.de/pub/likwid/likwid-4.3.4.tar.gz && \
  tar -xf likwid-4.3.4.tar.gz && \
  cd likwid-4.3.4 && \
  sed -i 's/\/usr\/local/\/usr\/local\/likwid/g' config.mk && \
  make && \
  make install && \
  cd .. && \
  export PATH="/usr/local/likwid/bin/:${PATH}" && \
  export PATH="/usr/local/likwid/sbin/:${PATH}" && \
  export LIKWID_INCLUDE="-I/usr/local/likwid/include" && \
  export LIKWID_LIB="-L/usr/local/likwid/lib"
