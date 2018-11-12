
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>


Hook(run);
InitVM();

int main() {
    vmNatHandler(nativeHook);
    run();
}

