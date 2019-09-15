#ifndef VM_H
#define VM_H

#include <stdint.h>
#include <stdbool.h>

/////////////////////
/// CUSTOMISATION ///
/////////////////////

// Space reserved for the stack of the vm in machine words
#ifndef DATA_STACK_DEPTH
#define DATA_STACK_DEPTH 120
#endif
// Space reserved for the call stack of the vm, it is kind of the recursion limit
#ifndef CALL_STACK_DEPTH
#define CALL_STACK_DEPTH 64
#endif

// A command used to debug the vm. 
#ifndef DEBUG_VM
#define DEBUG_VM(instr) //printf("[0x%x], {%x,%x,%x,%x,%x,%x,%x,%x} - (%x,%x,%x,%x,%x,%x,%x,%x)\n", instr, data[sp], data[sp-1], data[sp-2], data[sp-3], data[sp-4], data[sp-5], data[sp-6], data[sp-7], address[rp], address[rp-1], address[rp-2], address[rp-3], address[rp-4], address[rp-5], address[rp-6], address[rp-7]);
#endif

/////////////
/// TYPES ///
/////////////


/**
 * The machine word used by the vm. It is chosen to be 16bits and can hold up to 3 instuctions.
 * The 16bits width is chosen because of the word width of the C2000 mcu series.
 */
typedef int16_t CELL;
typedef uint16_t UCELL;
typedef int32_t LCELL;
typedef int32_t ULCELL;


/**
 * The environnement needed to run the vm.
 * To setup an environment, you only need to setup the memory.
 */
typedef struct vm_t {
    CELL sp; // Stack pointer
    CELL rp; // Adress pointer
    CELL ip; // Instruction pointer

    CELL data[DATA_STACK_DEPTH];
    CELL address[CALL_STACK_DEPTH];

    CELL memory_size; // The size of the rom chunk
    bool memory_writable; // Is the image writable (self modifying code)
    CELL* memory;

    bool dma; // Set by the recompiler when compiling, disable running code

    void (*nat_handler)(struct vm_t*); // Handler for native calls

    CELL* symbolTable;
    CELL** imageTable;
} vm_t;

////////////////////////////
/// OPERATIONS OF THE VM ///
////////////////////////////

/**
 * Cleanup the environment
 */
void vmClear(vm_t*);

/**
 * Run the vm until an end condition is triggered. The entry point can be selected
 */
bool vmRun(vm_t*, CELL);

/**
 * Load the memory into the vm
 */
void vmLoad(vm_t*, CELL* mem, CELL size, bool writable);
/**
 * Set the handler for native calls
 */
void vmNatHandler(vm_t*, void(*)(vm_t*));

/**
 * Get the cells on the top of the stack, used by library functions
 */
CELL vmTOS(vm_t*);
void vmLit(vm_t*, CELL);
void vmLit32(vm_t*, int32_t);
CELL vmPop(vm_t*);
int32_t vmPop32(vm_t*);
CELL vmNOS(vm_t*);


#endif // #ifndef VM_H
