FROM nvidia/cuda:10.1-devel-ubuntu18.04

ARG SBT_VERSION=1.2.8

RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-8-jdk python3 openmpi-bin openmpi-common libopenmpi-dev libopenmpi2 ant & \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion
