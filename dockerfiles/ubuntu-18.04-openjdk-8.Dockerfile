FROM ubuntu:18.04

ARG SBT_VERSION=1.2.8

# Install sbt
RUN \
  apt-get update && apt-get install -y build-essential curl git openjdk-8-jdk openjdk-8-source python3 llvm libclang-dev libgmp-dev libtool dh-autoreconf && \
  curl -L -o sbt-$SBT_VERSION.deb https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt && \
  sbt sbtVersion

