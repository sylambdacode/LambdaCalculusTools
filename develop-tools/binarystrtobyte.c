#include <stdio.h>

int main() {
    char c = 0;
    int i = 0;
    char output_c = 0;
    while ((c = getchar()) != EOF) {
        char bit;
        if (c == '1') {
            bit = 1;
        } else if (c == '0') {
            bit = 0;
        } else {
            continue;
        }
        output_c = (output_c << 1) | bit;
        i++;
        if (i == 8) {
            i = 0;
            printf("%c", output_c);
        }
    }
    return 0;
}