#!/bin/sh
cat lam/*.lam > .merged_code.lam
./develop-tools/bin/bytetobinarystr | cabal run lambda-calculus-tools -- run .merged_code.lam | ./develop-tools/bin/binarystrtobyte