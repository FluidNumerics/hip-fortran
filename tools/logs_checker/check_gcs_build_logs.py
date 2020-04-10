#!/usr/bin/python3

import json

logs_dir = "/logs"

def main():

    with open(logs_dir+"/results.json") as f:
        config = json.load(f)

    failed = False
    for test in config['testsuite']:

       obj = {'build_exit' : test['build']['exit_code'],
              'run_exit' : test['run']['exit_code'],
              'name' : test['name']}

       if test['build']['exit_code'] != 0:
          failed = True
       elif test['run']['exit_code'] != 0:
          failed = True

       print(json.dumps(obj, sort_keys=True, indent=2))
       

    if failed :
        print( 'hip-fortran testsuite failed.' )
        sys.exit(-1)
    else:
        print( 'hip-fortran testsuite passed!')


    
if __name__ == '__main__':

    main()
