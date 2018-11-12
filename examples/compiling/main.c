
#include "system.map.h"
#include "vm/vmpp.h"

#include <stdio.h>

HookR1(fibonacci);
InitVM();

int main() {
  printf("The 23rd fibonacci number is : %i", fibonacci(23));
}
