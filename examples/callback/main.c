
#include "vm/vm.h"
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>



vm_t vm;
Hook(&vm, run);
AllocVM();

void gc () { printf("Press a key:"); vmLit(&vm, getchar()); }
void f1() { puts("F1 is called when '1' is pressed\n"); }
void f2() { printf("The key %i was pressed\n", vmPop(&vm)); }

int main() {
    InitVM(&vm);
    vm.nat_handler = 0; // When using manual callback support, you need to reset the handler

    puts("The function called is chosen at runtime!\n");
    // We are stting symbols directly in memory
    Callback(&vm, run, gc, &gc);
    Callback(&vm, run, f1, &f1);
    Callback(&vm, run, f2, &f2);

    run();
}

