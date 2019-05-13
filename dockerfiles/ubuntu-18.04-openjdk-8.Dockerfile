FROM FROM nvidia/cuda:10.1-devel-ubuntu18.04

ARG SBT_VERSION=1.2.8

# Install sbt
RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-8-jdk python3 openmpi-bin openmpi-common libopenmpi-dev libopenmpi2 libgmp-dev libclang-dev libclang1 llvm libtool dh-autoreconf autoconf scala && \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion

