#include "vm/vm.h"

#ifdef DEBUG_VM 
#include <stdio.h>
#endif

////////////////
/// VM STATE ///
////////////////

// Instructions
enum vm_opcode {
    /* 0 */  VM_NOP,    /* do nothing */
    /* 1 */  VM_LIT,    /* insert next 16bit word in the source on the stack */
    /* 2 */  VM_DUP,    /* duplicate the word on top of the stack */
    /* 3 */  VM_DROP,   /* drop the word on top of the stack */
    /* 4 */  VM_SWAP,   /* swap the two words on top of the stack */
    /* 5 */  VM_PUSH,   /* push the word on top of the data stack to the address stack */
    /* 6 */  VM_POP,    /* pop the word on top of the address stack to the data stack */
    /* 7 */  VM_JUMP,   /* Jump to the location in the word on top of the stack */
    /* 8 */  VM_JNZ,    /* Jump to the location in the word on top of the stack if the second one is not zero */
    /* 9 */  VM_CALL,   /* Call to the location on top of the stack */
    /* A */  VM_CCALL,  /* Call to the location on top of the stack if the second one is not zero */
    /* B */  VM_NAT,    /* Call the native function whose identifier is the word on the top of the stack */
    /* C */  VM_RETURN, /* RET */
    /* D */  VM_EQ,     /* Equate (and consume) the two first words on the stack, then push a boolean */
    /* E */  VM_NEQ,    /* N-Equate (and consume) the two first words on the stack, then push a boolean */
    /* F */  VM_LT,     /* Compare (and consume) the two first words on the stack, then push a boolean */
    /* 10 */ VM_GT,     /* Compare (and consume) the two first words on the stack, then push a boolean */
    /* 11 */ VM_COPY,   /* Put the (first word on the stack)th value of the addres stack on the stack */
    /* 12 */ VM_PASTE,  /* Put (NOS) word from the stack onto the adress stack, starting from the (TOS)th.
    /* 13 */ VM_FETCH,  /* RESERVED */
    /* 14 */ VM_STORE,  /* RESERVED */
    /* 15 */ VM_ADD,    /* Add and consume the first two words on the stack and put the result */
    /* 16 */ VM_SUB,    /* Subtract and consume the first two words on the stack and put the result */
    /* 17 */ VM_MUL,    /* Multiply and consume the first two words on the stack and put the result */
    /* 18 */ VM_DIVMOD, /* TOS = NOS / TOS; NOS = NOS % TOS; */
    /* 19 */ VM_AND,    /* Perform bitwise and NOS & TOS */
    /* 1A */ VM_OR,     /* Perform bitwise or NOS | TOS */
    /* 1B */ VM_XOR,    /* Perform bitwise xor NOS ^ TOS */
    /* 1C */ VM_SHIFT,  /* Perform NOS >> TOS */
    /* 1D */ VM_ZRET,   /* Return if TOS is 0 */
    /* 1E */ VM_END     /* End the current VM execution */
};
#define NUM_OPS VM_END + 1

/**
 * The environnement needed to run the vm.
 * To setup an environment, you only need to setup the memory.
 */
static CELL sp; // Stack pointer
static CELL rp; // Adress pointer
static CELL ip; // Instruction pointer

// The stacks
static CELL data[DATA_STACK_DEPTH]; 
static CELL address[CALL_STACK_DEPTH];

static CELL memory_size; // The size of the rom chunk
static bool memory_writable; // Is the image writable (self modifying code)
static CELL* memory;

static bool dma; // Set by the recompiler when compiling, disable running code

static void (*nat_handler)(void); // Handler for native calls

#define TOS  data[sp]
#define NOS  data[sp-1]
#define TORS address[rp]

////////////////////
/// INSTRUCTIONS ///
////////////////////

static void inst_nop() {DEBUG_VM(VM_NOP)}
static void inst_lit() {sp++; ip++; TOS = memory[ip]; DEBUG_VM(VM_LIT)}
static void inst_dup() {sp++; data[sp] = NOS; DEBUG_VM(VM_DUP)}
static void inst_drop() {data[sp] = 0; if (--sp < 0) ip = memory_size;}
static void inst_swap() {int a; a = TOS; TOS = NOS; NOS = a; DEBUG_VM(VM_SWAP)}
static void inst_push() {rp++; TORS = TOS; inst_drop(); DEBUG_VM(VM_PUSH); }
static void inst_pop() {sp++; TOS = TORS; rp--; DEBUG_VM(VM_POP)}
static void inst_jump() {ip = TOS - 1; inst_drop(); DEBUG_VM(VM_JUMP);}
static void inst_jnz() {
    int a = TOS; inst_drop();
    int b = TOS; inst_drop();
    if (b!=0) ip = a-1;
    DEBUG_VM(VM_JNZ)
}
static void inst_call() {rp++; TORS = ip; ip = TOS - 1;  inst_drop(); DEBUG_VM(VM_CALL);}
static void inst_ccall() {
    int a, b;
    a = TOS; inst_drop();  /* False */
    b = TOS; inst_drop();  /* Flag  */
    if (b != 0) {
        rp++;
        TORS = ip;
        ip = a - 1;
    }
    DEBUG_VM(VM_CCALL)
}
static void inst_nat() {
    if (nat_handler != 0) {
        nat_handler();
    } else {
        int a = TOS; inst_drop();
        // Default handler, call from memory (not always possible)
        switch (a) {
        default: {
            void (**fp)(void) = (void (**)(void))(memory + a);
            if ((*fp) != 0) (*fp)(); break; }
        }
    }
    DEBUG_VM(VM_NAT)
        }
static void inst_return() {ip = TORS; rp--; DEBUG_VM(VM_RETURN)}
static void inst_eq() {NOS = (NOS == TOS) ? -1 : 0;  inst_drop();DEBUG_VM(VM_EQ); }
static void inst_neq() {NOS = (NOS != TOS) ? -1 : 0;  inst_drop();DEBUG_VM(VM_NEQ); }
static void inst_lt() {NOS = (NOS < TOS) ? -1 : 0;  inst_drop();DEBUG_VM(VM_LT); }
static void inst_gt() {NOS = (NOS > TOS) ? -1 : 0; inst_drop(); DEBUG_VM(VM_GT);}
static void inst_copy() {TOS = address[rp-TOS]; DEBUG_VM(VM_COPY);}
static void inst_paste() {
    int offset = TOS; inst_drop();
    int len = TOS; inst_drop();
    for(int i = 0; i < len; i++) {
        address[rp-(offset + i)] = data[sp-len+1+i];
    }
    DEBUG_VM(VM_PASTE);
}
static void inst_fetch() {
    switch (TOS) {
    case -1: TOS = sp - 1; break;
    case -2: TOS = rp; break;
    case -3: TOS = memory_size; break;
    default: TOS = memory[TOS]; break;
    }
    DEBUG_VM(VM_FETCH)
}
static void inst_store() {if(memory_writable) memory[TOS] = NOS; inst_drop(); inst_drop(); DEBUG_VM(VM_STORE)}
static void inst_add() {NOS += TOS; inst_drop(); DEBUG_VM(VM_ADD)}
static void inst_sub() {NOS -= TOS; inst_drop(); DEBUG_VM(VM_SUB)}
static void inst_mul() {NOS *= TOS; inst_drop(); DEBUG_VM(VM_MUL)}
static void inst_divmod() {
    int a, b;
    a = TOS;
    b = NOS;
    TOS = b / a;
    NOS = b % a;
    DEBUG_VM(VM_DIVMOD)
}
static void inst_and() {NOS = TOS & NOS; inst_drop(); DEBUG_VM(VM_AND)}
static void inst_or() {NOS = TOS | NOS; inst_drop(); DEBUG_VM(VM_OR)}
static void inst_xor() {NOS = TOS ^ NOS; inst_drop(); DEBUG_VM(VM_XOR)}
static void inst_shift() {
    CELL y = TOS;
    CELL x = NOS;
    if (TOS < 0)
        NOS = NOS << (TOS * -1);
    else {
        if (x < 0 && y > 0)
            NOS = x >> y | ~(~0U >> y);
        else
            NOS = x >> y;
    }
    inst_drop();
    DEBUG_VM(VM_SHIFT)
}
static void inst_zret() {
    if (TOS == 0) {
        inst_drop();
        ip = TORS;
        rp--;
    }
    DEBUG_VM(VM_ZRET)
}
static void inst_end() {ip = memory_size; DEBUG_VM(VM_END)}

typedef void (*Handler)(void);

Handler instructions[NUM_OPS] = {
    inst_nop, inst_lit, inst_dup, inst_drop, inst_swap, inst_push, inst_pop,
    inst_jump, inst_jnz, inst_call, inst_ccall, inst_nat, inst_return, inst_eq, inst_neq, inst_lt,
    inst_gt, inst_copy, inst_paste, inst_fetch, inst_store, inst_add, inst_sub, inst_mul, inst_divmod,
    inst_and, inst_or, inst_xor, inst_shift, inst_zret, inst_end
};

//////////////////////
/// Implementation ///
//////////////////////

void vmProcessOpcode(CELL opcode) {
    instructions[opcode]();
}

// NOTE This function depend on the size of CELL
int vmValidatePackedOpcodes(CELL opcode) {
    CELL raw = opcode;
    CELL current;
    int valid = -1;
    for (int i = 0; i < 3; i++) {
        current = raw & 31;
        if (!(current >= 0 && current < NUM_OPS))
            valid = 0;
        raw = raw >> 5;
    }
    return valid;
}

// NOTE This function depend on the size of CELL
void vmProcessPackedOpcodes(int opcode) {
    CELL raw = opcode;
    for (int i = 0; i < 3; i++) {
        vmProcessOpcode(raw & 31);
        raw = raw >> 5;
    }
}

bool vmRun(CELL from){
    CELL opcode, i, lastip;
    bool success = true;
    rp++; TORS = ip;
    rp++; TORS = memory_size; 
    ip=from;
    while(ip < memory_size) {
        opcode = memory[ip];
        if(vmValidatePackedOpcodes(opcode) != 0){
            vmProcessPackedOpcodes(opcode);
        } else if (opcode >= 0 && opcode < NUM_OPS) {
            vmProcessOpcode(opcode);
        } else {
            success = false;
            break;
        }
        ip++;
    }
    ip = TORS; rp--;

    return success;
}

CELL vmTOS() {return TOS;}
void vmLit(CELL c) {sp++; TOS=c;}
void vmLit32(int32_t c) {sp++; TOS=c>>16; sp++; TOS=c&0xFFFF; }
CELL vmPop() {CELL r = TOS; if(sp > 0) sp--; return r;}
int32_t vmPop32() {
    int32_t ret;
    ((UCELL*)(&ret))[0] = *((UCELL*)data+sp);
    ((CELL*)(&ret))[1] = *((CELL*)data+sp-1);
    if(sp > 0) sp--;  if(sp > 0) sp--;
    return ret;
}
CELL vmNOS() {return NOS;}

void vmNatHandler(void (*h)(void)) {
    nat_handler = h;
}

void vmLoad(CELL* mem, CELL size, bool writable) {
    memory = mem;
    memory_size = size;
    memory_writable = writable;
}

void vmClear() {
    ip = sp = rp = 0;

    if(memory_writable)
        for (ip = 0; ip < memory_size; ip++)
            memory[ip] = VM_NOP;

    for (ip = 0; ip < DATA_STACK_DEPTH; ip++)
        data[ip] = 0;

    for (ip = 0; ip < CALL_STACK_DEPTH; ip++)
        address[ip] = 0;
}
