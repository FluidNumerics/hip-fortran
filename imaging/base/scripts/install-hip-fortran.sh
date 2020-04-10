#!/bin/bash

git clone https://github.com/FluidNumerics/hip-fortran.git --depth 1 /tmp/hip-fortran

cd /tmp/hip-fortran && \
/tmp/hip-fortran/configure --enable-nvcc && \
make && make install

cp -r /tmp/hip-fortran/testsuite /opt/hip-fortran/
