#include <stdio.h>

int main() {
    char c;
    while ((c = getchar()) != EOF) {
        for (int i = 7; i >= 0; i--) {
            printf("%d", (c >> i) & 1);
        }
        fflush(stdout);
    }
    return 0;
}