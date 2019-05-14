FROM nvidia/cuda

ARG SBT_VERSION=1.2.8

# Install sbt
RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-8-jdk python3 openmpi-bin openmpi-common libopenmpi-dev libopenmpi2 ant scala
