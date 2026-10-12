#!/bin/sh

# Copyright (c) 2025 sylambdacode
# SPDX-License-Identifier: MIT



cd additional-utilities/binary-string-tools
./build.sh
cd ../..

cabal build
