/*
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
*/

#include <stdio.h>

int main() {
    char c;
    while ((c = getchar()) != EOF) {
        for (int i = 7; i >= 0; i--) {
            int bit = (c >> i) & 1;
            printf("%d", bit);
        }
        fflush(stdout);
    }
    return 0;
}
