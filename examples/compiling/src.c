
#include   "../../stdlib/stdlib.h"

@fibonacci
int32 main(int n) {
    n = n - 3;
    int32 a = 1L;
    int32 b = 2L;
    int32 tmp = 0L;
    do {
        tmp = a;
        a = b;
        b = add32(tmp,b);
        n = n - 1;
    } while(n > 0);
    return b;
}
