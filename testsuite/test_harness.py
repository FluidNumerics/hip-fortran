#!usr/bin/python3
DOC="""test_harness

Copyright 2020 Fluid Numerics
All Rights Reserved

A tool to orchestrate tests of hip-fortran

Usage: 
  test_harness run [--config=<string>] [--outdir=<string>]

Commands:
  run               Run tests in specified config

Options:
  -h --help            Display this help screen
  --config=<string>    Test configuration file [default: ./hftests.json]
  --outdir=<string>    Path to report test output [default: ./]
"""

import os
import subprocess
import shlex
import json
import re
from docopt import docopt
from datetime import datetime


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
    return proc.returncode, proc.stdout.decode('utf-8')

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
    results = {'date':datetime.utcnow().strftime("%m/%d/%Y, %H:%M:%S"),
               'build_id':BUILD_ID,
               'testsuite':[]}

    for test in config['testsuite']:
 
        stats = {}
        rc, stdout = call_host_process('make {}'.format(test['name']))
        stats['build'] = {'exit_code':rc,
                          'stdout':stdout}
        stats['run'] = {'exit_code':999,
                        'stdout':[],
                        'profile':[]}
        results['testsuite'].append( {'name':test['name'],
                                      'results':stats} )
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
                stats.append({'time_percent':float(formatted_line[0].strip('%')),
                              'time':numsplit.match(formatted_line[1]).groups()[0],
                              'time_units':numsplit.match(formatted_line[1]).groups()[1],
                              'calls':int(formatted_line[2]),
                              'avg_time':float(numsplit.match(formatted_line[3]).groups()[0]),
                              'avg_time_units':numsplit.match(formatted_line[3]).groups()[1],
                              'min_time':float(numsplit.match(formatted_line[4]).groups()[0]),
                              'min_time_units':numsplit.match(formatted_line[4]).groups()[1],
                              'max_time':float(numsplit.match(formatted_line[5]).groups()[0]),
                              'max_time_units':numsplit.match(formatted_line[5]).groups()[1],
                              'kernel_name':' '.join(formatted_line[6:])})

                 

            line = fp.readline()
            cnt += 1
    
    return stats

def profile(config, build_results):

    results = build_results
    if config['profiler'] == 'nvprof':
        k=0
        for test in build_results['testsuite']:
 
            if test['results']['build']['exit_code'] == 0:
                cmd = 'nvprof --log-file {PATH}/{TEST}.nvprof {PATH}/{TEST}'.format(TEST=test['name'],
                                                                                    PATH=config['install_path'])

                rc, stdout = call_host_process(cmd)
                results['testsuite'][k]['results']['run']['exit_code'] = rc
                results['testsuite'][k]['results']['run']['stdout'] = stdout

                if results['testsuite'][k]['results']['run']['exit_code'] == 0:
                    results['testsuite'][k]['results']['run']['profile'] = parse_nvprof_logfile(config,test)
                

            k+=1
    else:
        print('Profiler {} not currently supported'.format(config['profiler']))

    return results

#END profile

def main():

    args = parse_cli()

    if args['run'] :

       config = load_tests(args['--config'])
       
       results = build(config)

       results = profile(config,results)
       
       config = {'config':config,
                 'date':results['date'],
                 'build_id':BUILD_ID}
       f = open('{}/config.json'.format(args['--outdir']),'w')
       json.dump(config,f,sort_keys=True)
       f.close()

       f = open('{}/results.json'.format(args['--outdir']),'w')
       json.dump(results,f,sort_keys=True)
       f.close()
       

#END main

if __name__ == '__main__':
    main()

