
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>

Hook_1_2(fibonacci);
InitVM();

int main() {
  vmNatHandler(nativeHook);
  printf("The 46th fibonacci number is : %li", fibonacci(46));
}
