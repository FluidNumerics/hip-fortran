# hip-fortran/testsuite

## Prerequisites
The testsuite is executed using [`test_harness.py`](./test_harness.py). To run this application, you need `python3` and the `docopt` python package. If you're system has python3, you can set up a virtual environment to install the pre-requisites

```
$ python3 -m venv env
$ source ./env/bin/activate
(env) $ pip3 install docopt
```


## Using the test-suite
Once hip-fortran is installed on your system, you can modify `hftests.json` to match your system's configuration.
* For AMD GPU platforms, set `"hip_platform":"hcc"`; for Nvidia GPU platforms, set `"hip_platform":"nvcc"`.
* Set your fortran compiler using the `fc` variable.
* Set `cxx` to your `hipcc` compiler. If `hipcc` is in your `PATH`, then you can leave this as is.
* Lib must link to `libgfortran.a` (or the appropriate Fortran compiler library), `libstc++ (-lstdc++`, and `libmath.a (-lm)`. Add any necessary `-L` paths to make the location of these libraries known.
* Set the `install_path` to the location where the testsuite binaries will be installed

You can execute the test-suite by running
```
$ python3 -m venv env
$ source ./env/bin/activate
(env) $ pip3 install docopt
(env) $ python3 ./test_harness.py run
```
If you have another config file that matches the schema in [`hftests.json`](./hftests.json), you can use the `--config` flag, e.g.
```
(env) $ python3 ./test_harness.py run --config=/path/to/your/tests.json
```

## Singularity (for hip-fortran development)
Fluid Numerics develops hip-fortran using Google Cloud Build and a customized system for testing GPU accelerated applications. Cloud Build is used to create `hip-fortran` Docker container images. Successfully created `hip-fortran` containers are then passed to another Cloud Build run that converts the Docker image to a Singularity image with a test section. 

The singularity image can be easily tested on a host system with CUDA-Toolkit and ROCM-dev installed. For example, on Nvidia platforms, we can simply execute
```
singularity test --nv
```
On AMD platforms,
```
singularity test --rocm
```
