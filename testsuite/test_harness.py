#!usr/bin/python3
DOC="""test_harness

Copyright 2020 Fluid Numerics
All Rights Reserved

A tool to orchestrate tests of hip-fortran

Usage: 
  test_harness run [--config=<string>]

Commands:
  run               Run tests in specified config

Options:
  -h --help            Display this help screen
  --config=<string>    Test configuration file [default: ./hftests.json]
"""

import os
import subprocess
import shlex
import json
import re
from docopt import docopt


BUILD_ID=os.getenv('BUILD_ID')

def parse_cli():

    args = docopt(DOC,version='test_harness 0.0.0')
    return args

#END parse_cli

def load_tests(config_json):

    with open(config_json) as f:
        config = json.load(f)

    return config

#END load_tests

def call_host_process(cmd):

    proc = subprocess.run(shlex.split(cmd), 
                          stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT)

#    print(proc)
    return proc.returncode

#END call_host_process

def build(config):

    os.environ['HIP_PLATFORM'] = config['hip_platform']
    os.environ['FC'] = config['fc']
    os.environ['CXX'] = config['cxx']
    os.environ['CXXFLAGS'] = config['cxxflags']
    os.environ['LIB'] = config['lib']
    os.environ['INCLUDE'] = config['include']
    os.environ['OPT_FLAGS'] = config['opt']
    os.environ['INSTALL_PATH'] = config['install_path']
    
    exit_code = call_host_process('mkdir -p {}'.format(config['install_path']))

    k=0
    results = config
    for test in config['testsuite']:
 
        stats = {}
        stats['build_exit_code'] = call_host_process('make {}'.format(test['name']))
        stats['run_exit_code'] = 999
        results['testsuite'][k]['results'] = stats
        k+=1

    return results


#END build

def parse_nvprof_logfile(config,test):

    filepath = '{PATH}/{TEST}.nvprof'.format(TEST=test['name'],PATH=config['install_path'])

    numsplit = re.compile("([.0-9]+)([a-zA-Z]+)")
    pull_info = False
    stats = []

    with open(filepath) as fp:
        line = fp.readline()
        cnt = 1
        while line:
            if line.strip().startswith('Type'):
                pull_info = True
                line = fp.readline()
                cnt +=1
                continue

            if pull_info: 
                formatted_line=line.strip().split(':')[-1].lstrip().split()
#                print("Line {}: {}".format(cnt,formatted_line[6:]))
                stats.append({'time_percent':float(formatted_line[0].strip('%')),
                              'time':numsplit.match(formatted_line[1]).groups()[0],
                              'time_units':numsplit.match(formatted_line[1]).groups()[1],
                              'calls':int(formatted_line[2]),
                              'avg_time':numsplit.match(formatted_line[3]).groups()[0],
                              'avg_time_units':numsplit.match(formatted_line[3]).groups()[1],
                              'min_time':numsplit.match(formatted_line[4]).groups()[0],
                              'min_time_units':numsplit.match(formatted_line[4]).groups()[1],
                              'max_time':numsplit.match(formatted_line[5]).groups()[0],
                              'max_time_units':numsplit.match(formatted_line[5]).groups()[1],
                              'kernel_name':' '.join(formatted_line[6:])})

                 

            line = fp.readline()
            cnt += 1
    
    return stats

def profile(config):

    results = config
    if config['profiler'] == 'nvprof':
        k=0
        for test in config['testsuite']:
 
            if test['results']['build_exit_code'] == 0:
                cmd = 'nvprof --log-file {PATH}/{TEST}.nvprof {PATH}/{TEST}'.format(TEST=test['name'],
                                                                                    PATH=config['install_path'])

                results['testsuite'][k]['results']['run_exit_code'] = call_host_process(cmd)

                if results['testsuite'][k]['results']['run_exit_code'] == 0:
                    results['testsuite'][k]['results']['profiles'] = parse_nvprof_logfile(config,test)
                

            k+=1
    else:
        print('Profiler {} not currently supported'.format(config['profiler']))

    return results

#END profile

def main():

    args = parse_cli()

    if args['run'] :

       config = load_tests(args['--config'])
       
       config = build(config)

       config = profile(config)
       
       print(json.dumps(config,sort_keys=True,indent=2))
       

#END main

if __name__ == '__main__':
    main()
