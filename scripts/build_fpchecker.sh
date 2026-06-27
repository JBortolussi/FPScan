# Build FPChecker
cd FPChecker
mkdir build
sed "s/llvm-config/llvm-config-19/" CMakeLists.txt -i
sed "s/\"clang\"/\"clang-19\"/" CMakeLists.txt -i
sed "s/\"clang++\"/\"clang++-19\"/" CMakeLists.txt -i
cd build
mkdir install
cmake -DCMAKE_INSTALL_PREFIX=install ..
make && make install
cd ../..

# Make FPChecker available
ln -s FPChecker/build/install FPChecker_install