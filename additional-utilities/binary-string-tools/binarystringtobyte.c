/*
Copyright (c) 2025 sylambdacode
SPDX-License-Identifier: MIT
*/

#include <stdio.h>

int main() {
    char c = 0;
    int i = 0;
    char result_char = 0;
    while ((c = getchar()) != EOF) {
        char bit;
        if (c == '1') {
            bit = 1;
        } else if (c == '0') {
            bit = 0;
        } else {
            continue;
        }
        result_char = (result_char << 1) | bit;
        i++;
        if (i == 8) {
            i = 0;
            printf("%c", result_char);
            fflush(stdout);
        }
    }
    return 0;
}
