#!/bin/sh

set -e

rm -f Makefile
rm -rf Gt
bnfc --functor --haskell -d -m gt.cf
sed -i "10s/^$/GHC_OPTS = -package array/" Makefile
make -j
mkdir -p Parser/
cp -f Gt/*.hs Parser/
rm Parser/Test.hs
sed -i 's/Gt/Parser/g' Parser/*.hs
rm -rf ../src/Parser
mv Parser ../src/
