set -e

rm -rf build
cmake -S lpng1640 -B build
cd build
make
