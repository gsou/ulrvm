
#include "system.map.h"
#include "vm/vmpp.h"

#include <iostream>

HookR1(fibonacci);
InitVM();

int main() {
    // Import code
    CELL loc; std::cin >> loc;
    CELL len; std::cin >> len;
    for(int i = 0; i < len; i++) {
        CELL source; std::cin >> source;
        fibonacci_source[i] = source;
    }

    // Function call
    std::cout << "The 23rd fibonacci number is : " << fibonacci(23);
}
