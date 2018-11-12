#ifndef VM_H
#define VM_H

#include <stdint.h>
#include <stdbool.h>

/////////////////////
/// CUSTOMISATION ///
/////////////////////

// Space reserved for the stack of the vm in machine words
#define DATA_STACK_DEPTH 64
// Space reserved for the call stack of the vm, it is kind of the recursion limit
#define CALL_STACK_DEPTH 64

// A command used to debug the vm. 
#define DEBUG_VM(instr) // printf("[0x%x], TOS: %i, NOS: %i\n", instr, TOS, NOS);

/////////////
/// TYPES ///
/////////////


/**
 * The machine word used by the vm. It is chosen to be 16bits and can hold up to 3 instuctions.
 * The 16bits width is chosen because of the word width of the C2000 mcu series.
 */
typedef int16_t CELL;

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
CELL vmPop(void);
CELL vmNOS(void);


#endif // #ifndef VM_H
