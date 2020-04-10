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



def load_build_config():

    build_config = {}
    scripts_path = '/scripts'
    GOOGLE_URL = "http://metadata.google.internal/computeMetadata/v1/instance/attributes"

    metadata = ['build_id',
                'gcs_bucket']

    for meta in metadata:
        req = urllib2.Request("{}/{}".format(GOOGLE_URL, meta))
        req.add_header('Metadata-Flavor', 'Google')
        resp = urllib2.urlopen(req)
        build_config[meta] = resp.read()

    print( build_config )
    return build_config

def copy_results_to_bucket():

    build_config = load_build_config( )
    subprocess.run(shlex.split('gsutil cp /opt/hip-fortran/results.json {BUCKET}/{PATH}/results.json'.format(BUCKET=build_config['gcs_bucket'],
                                                                                                             PATH=config['build_id'])))

def install_hip_fortran():

    subprocess.run(shlex.split('/scripts/install-hip-fortran.sh'),shell=True )

#END install_hip_fortran

def run_test_harness():
   
    build_config = load_build_config( )
    os.environ['BUILD_ID'] = build_config['build_id']
    hfpath = '/opt/hip-fortran/testsuite'
    subprocess.run(shlex.split('python3 {PATH}/test_harness.py run --config={PATH}/hftests.json --outdir=/opt/hip-fortran')

#END run_test_harness

def main():
    
    install_hip_fortran()

    run_test_harness()

    copy_results_to_bucket()

    #copy_results_to_bq()


# END main()

if __name__ == '__main__':

    try:
        main()
        print('Startup script success!')
        copy_logs_to_bucket()
    except:
        print('Startup script failed!')
        copy_logs_to_bucket()
