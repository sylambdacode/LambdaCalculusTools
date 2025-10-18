#!/bin/sh
cat lam/* > .merged_code.lam; ./develop-tools/bin/bytetobinarystr | cabal run lambda-calculus-tools -- .merged_code.lam | ./develop-tools/bin/binarystrtobyte