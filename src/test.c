#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rsa.h"

int main() {
    char pub1[32];
    char pub2[32];
    char priv1[32];
    char priv2[32];

    gen(pub1, priv1);
    gen(pub2, priv2);

    printf("Pair 1: (%016llx, %016llx)\n", pub1, priv1);
    printf("Pair 2: (%016llx, %016llx)\n", pub2, priv2);

    return 0;
}