FROM nvidia/cuda:10.1-devel-ubuntu18.04

ARG SBT_VERSION=1.3.2

RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-11-jdk python3 python3-pip openmpi-bin openmpi-common libopenmpi-dev libopenmpi2 ant mlocate time gfortran && \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion && \
  python3 -m pip install sympy numpy
