#include "vm/vm.h"
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>

vm_t vm;
Hook(&vm, run);
AllocVM();


int main() {
    InitVM(&vm);

    run();

}

