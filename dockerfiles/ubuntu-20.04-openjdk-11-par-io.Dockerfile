FROM i10git.cs.fau.de:5005/exastencils/exastencils/ubuntu-20.04-openjdk-11

# hdf5
RUN wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.12/hdf5-1.12.0/src/hdf5-1.12.0.tar.gz && \
    tar xvf hdf5-1.12.0.tar.gz && \
    cd hdf5-1.12.0 && \
    CC=mpicc ./configure --enable-parallel --prefix=/opt/hdf5 && \
    make && \
    make install && \
    HDF5_HOME=/opt/hdf5 ; export HDF5_HOME && \
    PATH=$HDF5_HOME/bin:$PATH ; export PATH && \
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF5_HOME/lib ; export LD_LIBRARY_PATH

# sionlib
RUN curl "http://apps.fz-juelich.de/jsc/sionlib/download.php?version=1.7.6" -o sionlib.tar.gz && \
    tar xvf sionlib.tar.gz && \
    cd sionlib && \
    ./configure --prefix=/opt/sionlib --disable-fortran --mpi=openmpi && \
    cd build-linux-gomp-openmpi && \
    make && \
    make install && \
    SION_HOME=/opt/sionlib ; export SION_HOME && \
    PATH=$SION_HOME/bin:$PATH ; export PATH && \
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SION_HOME/lib ; export LD_LIBRARY_PATH

# pnetcdf
RUN wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/pnetcdf-1.12.1.tar.gz && \
    tar xvf pnetcdf-1.12.1.tar.gz && \
    cd pnetcdf-1.12.1 && \
    CC=mpicc ./configure --prefix=/opt/PnetCDF && \
    make -j 8 && \
    make install && \
    PNETCDF_HOME=/opt/PnetCDF ; export PNETCDF_HOME && \
    PATH=$PNETCDF_HOME/bin:$PATH ; export PATH && \
    LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PNETCDF_HOME/lib ; export LD_LIBRARY_PATH

