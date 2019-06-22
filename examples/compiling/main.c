
#include "vm/vm.h"
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>

vm_t vm;
Hook_1_2(&vm, fibonacci);
AllocVM();

int main() {
  InitVM(&vm);
  printf("The 46th fibonacci number is : %li", fibonacci(46));
}
