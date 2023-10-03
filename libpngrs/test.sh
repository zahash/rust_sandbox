set -e

export srcdir="/home/zahash/code/sandboxrs/libpngrs/lpng1640"

cd build

echo "*********   START   *********"

sh $srcdir/tests/pngimage-quick

sh $srcdir/tests/pngstest-1.8
sh $srcdir/tests/pngstest-1.8-alpha
sh $srcdir/tests/pngstest-linear
sh $srcdir/tests/pngstest-linear-alpha
sh $srcdir/tests/pngstest-none
sh $srcdir/tests/pngstest-none-alpha
sh $srcdir/tests/pngstest-sRGB
sh $srcdir/tests/pngstest-sRGB-alpha

sh $srcdir/tests/pngunknown-IDAT
sh $srcdir/tests/pngunknown-discard
sh $srcdir/tests/pngunknown-if-safe
sh $srcdir/tests/pngunknown-sAPI
sh $srcdir/tests/pngunknown-sTER
sh $srcdir/tests/pngunknown-save
sh $srcdir/tests/pngunknown-vpAg

sh $srcdir/tests/pngvalid-progressive-interlace-standard
sh $srcdir/tests/pngvalid-progressive-size
sh $srcdir/tests/pngvalid-progressive-standard
sh $srcdir/tests/pngvalid-standard
sh $srcdir/tests/pngvalid-transform

sh $srcdir/tests/pngvalid-gamma-16-to-8
sh $srcdir/tests/pngvalid-gamma-alpha-mode
sh $srcdir/tests/pngvalid-gamma-background
sh $srcdir/tests/pngvalid-gamma-expand16-alpha-mode
sh $srcdir/tests/pngvalid-gamma-expand16-background
sh $srcdir/tests/pngvalid-gamma-expand16-transform
sh $srcdir/tests/pngvalid-gamma-sbit
sh $srcdir/tests/pngvalid-gamma-threshold
sh $srcdir/tests/pngvalid-gamma-transform

# sh $srcdir/tests/pngimage-full # too slow
# sh $srcdir/tests/pngtest-all # expected to crash


echo "*********   DONE   *********"
