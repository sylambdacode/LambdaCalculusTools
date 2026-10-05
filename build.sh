#!/bin/sh

# Copyright (c) 2025 sylambdacode
# SPDX-License-Identifier: MIT



cd additional-tools
./build.sh
cd ..

cabal build
