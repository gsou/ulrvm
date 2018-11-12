
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>

void gc () { printf("Press a key:"); vmLit(getchar()); }
void f1() { puts("F1 is called when '1' is pressed\n"); }
void f2() { printf("The key %i was pressed\n", vmPop()); }

Hook(run);
InitVM();

int main() {
    puts("The function called is chosen at runtime!\n");
    // We are stting symbols directly in memory
    Callback(run, gc, &gc);
    Callback(run, f1, &f1);
    Callback(run, f2, &f2);

    run();
}

