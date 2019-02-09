#ifndef VM_H
#define VM_H

#include <stdint.h>
#include <stdbool.h>

/////////////////////
/// CUSTOMISATION ///
/////////////////////

// Space reserved for the stack of the vm in machine words
#ifndef DATA_STACK_DEPTH
#define DATA_STACK_DEPTH 64
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

////////////
/// VARS ///
////////////

extern CELL symbolTable[];
extern CELL* imageTable[];

////////////////////////////
/// OPERATIONS OF THE VM ///
////////////////////////////

/**
 * Cleanup the environment
 */
void vmClear();

/**
 * Run the vm until an end condition is triggered. The entry point can be selected
 */
bool vmRun(CELL);

/**
 * Load the memory into the vm
 */
void vmLoad(CELL* mem, CELL size, bool writable);
/**
 * Set the handler for native calls
 */
void vmNatHandler(void(*)(void));

/**
 * Get the cells on the top of the stack, used by library functions
 */
CELL vmTOS(void);
void vmLit(CELL);
void vmLit32(int32_t);
CELL vmPop(void);
int32_t vmPop32(void);
CELL vmNOS(void);


#endif // #ifndef VM_H
