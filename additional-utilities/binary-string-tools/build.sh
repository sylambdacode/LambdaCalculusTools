#!/bin/sh

# Copyright (c) 2025 sylambdacode
# SPDX-License-Identifier: MIT


mkdir build
gcc binarystringtobyte.c -o build/binarystringtobyte
gcc bytetobinarystring.c -o build/bytetobinarystring
