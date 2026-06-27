cd fpscan
autoconf
./configure
dune build
cd ..

# make FPScan and helpers tools available
ln -s fpscan/_build/default/bin/main_fpscan.exe FPScan
ln -s fpscan/_build/default/bin/main_fpcore_to_C.exe FPCore_to_C
