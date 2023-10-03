set -e

LIB_NAME="kilors"

cbindgen --lang c --config cbindgen.toml --crate $LIB_NAME --output ./include/$LIB_NAME/$LIB_NAME.h

cargo clean
cargo build --release

mkdir -p ./lib/
cp ../target/release/lib$LIB_NAME.a ./lib/ | true
cp ../target/release/lib$LIB_NAME.so ./lib/ | true

rm -f kilo
gcc -o kilo kilo.c -Wall -W -pedantic -std=c99 -I./include/ -L./lib/ -l:lib$LIB_NAME.a # staticlink
