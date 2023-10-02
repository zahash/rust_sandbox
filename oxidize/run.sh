set -e

LIB_NAME="oxidize"

# GENERATE HEADERS
# requires cbindgen.toml to be present (file can be empty).
cbindgen --lang c --config cbindgen.toml --crate $LIB_NAME --output ./include/$LIB_NAME/$LIB_NAME.h

# CREATE .so (cdylib) or .a (staticlib) in target/release
cargo build --release
mkdir -p ./lib/
# | true prevents crash if file not found
cp ../target/release/*.a ./lib/ | true
cp ../target/release/*.so ./lib/ | true

rm main -f
# The -I flag tells gcc where to look for the header files
# The -L flag where to look for the library
# and the -l flag specifies the name of the library
gcc -Wall -Wextra -Werror -o main main.c -I./include/ -L./lib/ -l:lib$LIB_NAME.a # staticlink
# gcc -Wall -Wextra -Werror -o main main.c -I./include/ -L./lib/ -l$LIB_NAME # dynlink


# LD_LIBRARY_PATH not required for staticlib
# export LD_LIBRARY_PATH=./lib/:$LD_LIBRARY_PATH
./main

rm main -f
