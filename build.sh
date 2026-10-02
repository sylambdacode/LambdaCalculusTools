#!/bin/sh

# Copyright (c) 2025 sylambdacode
# SPDX-License-Identifier: MIT



cd develop-tools
./build.sh
cd ..

cabal build
