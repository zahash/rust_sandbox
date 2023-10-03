set -e

rm -rf build
rm -rf ./lib
rm -rf ./include

cmake -S lpng1640 -B build -DCMAKE_INSTALL_PREFIX="./"

cd build
make
make install
cd ..

rm -rf build
rm -rf ./bin
rm -rf ./share
