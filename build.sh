#!/bin/sh

cd develop-tools
./build.sh
cd ..

cabal build
