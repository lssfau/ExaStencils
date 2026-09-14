ARG BASE_IMAGE=gitos.rrze.fau.de:5005/cs10/exastencils/exastencils/ubuntu-24.04-openjdk-11
FROM ${BASE_IMAGE}

# HIP for AMD platforms

ARG ROCM_VERSION=6.3

RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://repo.radeon.com/rocm/rocm.gpg.key -o /etc/apt/keyrings/rocm.asc && \
    (echo "deb [arch=amd64 signed-by=/etc/apt/keyrings/rocm.asc] https://repo.radeon.com/rocm/apt/$ROCM_VERSION noble main" \
      | tee /etc/apt/sources.list.d/rocm.list) && \
    (printf "Package: *\nPin: release o=repo.radeon.com\nPin-Priority: 600\n" \
      | tee /etc/apt/preferences.d/rocm-pin-600) && \
    apt-get update && \
    apt-get install -y --no-install-recommends hipcc hip-dev hipcub-dev openmp-extras-dev && \
    ln -sfn "$(ls -d /opt/rocm-*)" /opt/rocm && \
    (echo "/opt/rocm/lib" | tee /etc/ld.so.conf.d/rocm.conf) && \
    ldconfig && \
    rm -rf /var/lib/apt/lists/*

ENV ROCM_PATH="/opt/rocm"
ENV HIP_PATH="/opt/rocm"

ENV CPATH="/opt/rocm/include${CPATH:+:${CPATH}}"
ENV LIBRARY_PATH="/opt/rocm/lib${LIBRARY_PATH:+:${LIBRARY_PATH}}"
ENV CMAKE_PREFIX_PATH="/opt/rocm${CMAKE_PREFIX_PATH:+:${CMAKE_PREFIX_PATH}}"

# make hipcc target AMD devices
ENV HIP_PLATFORM="amd"
ENV PATH="${ROCM_PATH}/bin:${PATH}"
