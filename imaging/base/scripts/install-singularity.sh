#!/bin/bash


# Install packages
    yum groupinstall -y 'Development Tools'

    yum install -y bind-utils \
                epel-release \
                gcc \
                git \
                numactl \
                numactl-devel \
                openssl-devel \
                libuuid-devel \
                libseccomp-devel \
                squashfs-tools \
                vim \
                wget 

# Install go
    if [ ! -d /usr/local/go ]; then
        cwd=$(pwd)
        cd /tmp
        
        curl -LO https://storage.googleapis.com/golang/go1.12.6.linux-amd64.tar.gz
        tar -C /usr/local -xvzf go1.12.6.linux-amd64.tar.gz
        echo 'export PATH=$PATH:/usr/local/go/bin' > /etc/profile.d/gopath.sh
        
        export HOME="/apps"
        export GOPATH="/apps/go"
        mkdir -p /apps/go
        export PATH=$PATH:/usr/local/go/bin
        go get -u github.com/golang/dep/cmd/dep
        cd $cwd
    fi


# install singularity
    if [ ! -d /apps/go/src/github.com/sylabs ]; then
        mkdir -p /apps/go/src/github.com/sylabs/

        git clone --branch v3.2.1 --depth 1 https://github.com/sylabs/singularity /apps/go/src/github.com/sylabs/singularity

        cd /apps/go/src/github.com/sylabs/singularity && \
        ./mconfig --prefix=/apps/singularity && \
        cd ./builddir && \
        make && \
        make install

        cd $cwd
    fi

# Add modulefile for singularity
mkdir -p /apps/modulefiles/singularity
cat <<EOF > /apps/modulefiles/singularity/3.2.1
#%Module 1.0
#
#  OpenMPI module for use with 'environment-modules' package:
#
conflict                singularity
prepend-path            PATH             /apps/singularity/bin/
setenv	SINGULARITY_VER 3.2.1
EOF
