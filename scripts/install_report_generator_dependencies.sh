apt install -y python3.12-venv

python3 -m venv ./venv
ln -s venv/bin/activate activate
source venv/bin/activate activate
python -m pip install matplotlib
python -m pip install pandas
python -m pip install pyyaml

apt install -y texlive

cd report_generator
rm fpscan.exe
ln -s ../FPScan fpscan.exe
rm FPChecker_install
ln -s ../FPChecker_install FPChecker_install
rm fpcore_to_C.exe
ln -s ../FPCore_to_C fpcore_to_C.exe

# patch clang to use clang-19 (same as fpchecker)
sed "s/clang/clang-19/" src/fpchecker/fpchecker.py -i