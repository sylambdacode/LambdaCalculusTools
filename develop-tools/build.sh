#!/bin/sh

# Copyright (c) 2025 sylambdacode
# SPDX-License-Identifier: MIT


mkdir build
gcc binarystrtobyte.c -o build/binarystrtobyte
gcc bytetobinarystr.c -o build/bytetobinarystr
