
struct {
  int a;
  int b;
} vec2;

#include "vm/vm.h"
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>

vm_t vm;
Hook(&vm, run);
AllocVM();


int main() {
    InitVM(&vm);

    vec2.a = 1000;
    vec2.b = -10000;

    run();

    printf("(%i, %i)\n", vec2.a, vec2.b);
}

