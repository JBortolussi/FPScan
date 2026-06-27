# FPScan SYNASC Artifact

## Content

This repositories contains the sources necessary to rebuild all the figures presented in the paper. It also a Dockerfile and installation scripts to make it easier to use. 

* FPChecker directory contains the sources of FPChecker
* fpscan directory contains the sources of FPScan
* report_generator directory contains the python module used to generate the figures
* scripts directory contains scripts used to build and setup the tools

## Build and setup the tools

### Docker
This is the recommend method as it limits environment issues. The provided Dockerfile can be used to build a Docker image with all the dependencies installed.

To create the docker image is long because of the installation of z3.

### FPChecker

The dependencies can be installed using:
```
scripts/install_fpchecker_dependencies.sh
```
FPChecker can be built using:
```
scripts/build_fpchecker.sh
```
Note: the build scripts should only be run once as it tweaks FPChecker sources.

### FPScan

The dependencies can be installed using:
```
./scripts/install_fpscan_dependencies.sh
```
FPChecker can be built using:
```
./scripts/build_fpscan.sh
```

### report_generator

All the dependencies can be installed using:
```
./scripts/install_report_generator_dependencies.sh
```
Note that this command must be run manually also on docker instances.

## Generate Figures

The first step first step is to install all the dependencies of FPScan and FPChecker either using the docker file, the provided installation scripts of the manually. Then both FPChecker and FPScan need to be build using the provided scripts. The the report_generator can be setup using the provided script.

Once all the tools are ready, the figure displayed in the article can be generated. All the command of the report_generator provide option to erase the cached result of each tools:

* `--rebuild_fpscan`: force the run of FPScan analysis
* `--rebuild_fpchecker`: force the run of FPChecker analysis
* `--rebuild_bitblasting`: orce the run of Bitblasting analysis

All the flowing requires the venv to be active:
```
source /app/venv/bin/activate
```

### Table I
Table I can be built using the following command inside the report_generator folder:
```
python -m src.main fpscan
```
The resulting table is named `table_1.pdf`.

Note that the table format are different but the data are the same.

### Figure 6
Figure 6 can be built using the following command inside the report_generator folder:
```
python -m src.main fps_fpc
```
The resulting figure is name `figure_6.pdf`.

### Figure 7
Figure 7 can be built using the following command inside the report_generator folder:
```
python -m src.main tps
```
The resulting figure is named `figure_7.pdf`

## Use FPScan
FPScan can be run using `/app/FPScan`. Example programs can be found under `/app/fpscan/examples/fpcore/prgm`. For instance:
```
/app/FPScan app/fpscan/examples/fpcore/prgm/sum.fpcore
```