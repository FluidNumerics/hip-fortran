#!/usr/bin/python
# Copyright 2020 Fluid Numerics LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import httplib
import os
import shlex
import socket
import subprocess
import time
import urllib
import urllib2



MOTD_HEADER = '''
The hip-fortran image contains the following open source 3rd party applications:

 o OpenHPC       ( https://github.com/openhpc/ohpc )
 o Singularity   ( https://github.com/sylabs/singularity )
 o ROCm          ( https://www.amd.com/en/graphics/servers-solutions-rocm ) 
 o CUDA-Repo     ( http://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/cuda-repo-rhel7-10.0.130-1.x86_64.rpm )
 o HIP-Fortran   ( https://github.com/fluidnumerics/hip-fortran )

Licenses for each 3rd party application can be found under /license and /opt
This software contains source code provided by NVIDIA Corporation.
Visit https://help.fluidnumerics.com/hip-fortran for more information
'''

def setup_motd():

    msg = MOTD_HEADER
    f = open('/etc/motd', 'w')
    f.write(msg)
    f.close()

# END setup_motd()

def install_packages():

        packages = ['epel-release',
                    'gcc',
                    'gcc-c++',
                    'gcc-gfortran',
                    'git',
                    'python3',
                    'python3-devel',
                    'python3-pip'
                   ]

        while subprocess.call(['yum', 'install', '-y'] + packages):
            print "yum failed to install packages. Trying again in 5 seconds"
            time.sleep(5)

        while subprocess.call(['pip', 'install', '--upgrade',
            'google-api-python-client', 'oauth2client','google-cloud','google-cloud-pubsub','cython', 'pyyaml', 'pprint', 'parse', 'docopt']):
            print "failed to install google python api client. Trying again 5 seconds."
            time.sleep(5)
            
        while subprocess.call(['pip3', 'install', '--upgrade',
            'google-api-python-client', 'oauth2client','google-cloud','google-cloud-pubsub','cython', 'pyyaml', 'pprint', 'parse', 'docopt','jsonschema', 'dictdiffer']):
            print "failed to install google python api client. Trying again 5 seconds."
            time.sleep(5)


#END install_packages()

def install_nvidia_drivers():


      f = open('/etc/profile.d/cuda.sh', 'w')
      f.write("""
CUDA_PATH=/usr/local/cuda
PATH=$CUDA_PATH/bin${PATH:+:${PATH}}
LD_LIBRARY_PATH=$CUDA_PATH/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
""")
      f.close()

      rpm = "cuda-repo-rhel7-10.0.130-1.x86_64.rpm"
      subprocess.call("apt-get -y install kernel-devel-$(uname -r) kernel-headers-$(uname -r)", shell=True)
      subprocess.call(shlex.split("wget http://developer.download.nvidia.com/compute/cuda/repos/rhel7/x86_64/" + rpm))
      subprocess.call(shlex.split("rpm -i " + rpm))
      subprocess.call(shlex.split("yum clean all"))
      subprocess.call(shlex.split("yum -y install cuda"))


def setup_stackdriver():

    subprocess.call(shlex.split("curl -sSO https://dl.google.com/cloudagents/install-monitoring-agent.sh"))
    subprocess.call(shlex.split("bash install-monitoring-agent.sh"))
    subprocess.call(shlex.split("curl -sSO https://dl.google.com/cloudagents/install-logging-agent.sh"))
    subprocess.call(shlex.split("bash install-logging-agent.sh"))

#END setup_stackdriver

def have_internet():
    conn = httplib.HTTPConnection("www.google.com", timeout=1)
    try:
        conn.request("HEAD", "/")
        conn.close()
        return True
    except:
        conn.close()
        return False

#END have_internet()

def install_singularity():

    subprocess.check_call(shlex.split('/scripts/install-singularity.sh'),shell=True )

#END install_singularity


def setup_license_directory():

    subprocess.call(shlex.split('mkdir -p /license'))
    
    # Link Singularity License
    subprocess.call(shlex.split('ln -s /go/src/github.com/sylabs/singularity/LICENSE.md /license/LICENSE.singularity') )

    # Link Nvidia EULA
    subprocess.call(shlex.split('wget -P /license/ http://developer.download.nvidia.com/compute/cuda/3_2_prod/toolkit/docs/EULA.txt') )
    subprocess.call(shlex.split('mv /license/EULA.txt /license/NVIDIA-EULA.txt') )


def main():
    
    setup_motd()

    while not have_internet():
        print "Waiting for internet connection"
    
    setup_stackdriver()

    install_packages()

    install_nvidia_drivers()

    install_singularity()

# END main()

if __name__ == '__main__':

    try:
        main()
        print('Startup script success!')
        copy_logs_to_bucket()
    except:
        print('Startup script failed!')
        copy_logs_to_bucket()
