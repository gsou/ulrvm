#include "vm/vm.h"

#ifdef DEBUG_VM
#include <stdio.h>
#endif
#include <string.h>

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


#define TOS  vm->data[vm->sp]
#define NOS  vm->data[vm->sp-1]
#define TORS vm->address[vm->rp]

////////////////////
/// INSTRUCTIONS ///
////////////////////

static void inst_nop(vm_t* vm) {DEBUG_VM(VM_NOP)}
static void inst_lit(vm_t* vm) {vm->sp++; vm->ip++; TOS = vm->memory[vm->ip]; DEBUG_VM(VM_LIT)}
static void inst_dup(vm_t* vm) {vm->sp++; vm->data[vm->sp] = NOS; DEBUG_VM(VM_DUP)}
static void inst_drop(vm_t* vm) {vm->data[vm->sp] = 0; if (--vm->sp < 0) vm->ip = vm->memory_size;}
static void inst_swap(vm_t* vm) {int a; a = TOS; TOS = NOS; NOS = a; DEBUG_VM(VM_SWAP)}
static void inst_push(vm_t* vm) {vm->rp++; TORS = TOS; inst_drop(vm); DEBUG_VM(VM_PUSH); }
static void inst_pop(vm_t* vm) {vm->sp++; TOS = TORS; vm->rp--; DEBUG_VM(VM_POP)}
static void inst_jump(vm_t* vm) {vm->ip = TOS - 1; inst_drop(vm); DEBUG_VM(VM_JUMP);}
static void inst_jnz(vm_t* vm) {
    int a = TOS; inst_drop(vm);
    int b = TOS; inst_drop(vm);
    if (b!=0) vm->ip = a-1;
    DEBUG_VM(VM_JNZ)
}
static void inst_call(vm_t* vm) {vm->rp++; TORS = vm->ip; vm->ip = TOS - 1;  inst_drop(vm); DEBUG_VM(VM_CALL);}
static void inst_ccall(vm_t* vm) {
    int a, b;
    a = TOS; inst_drop(vm);  /* False */
    b = TOS; inst_drop(vm);  /* Flag  */
    if (b != 0) {
        vm->rp++;
        TORS = vm->ip;
        vm->ip = a - 1;
    }
    DEBUG_VM(VM_CCALL)
}
static void inst_nat(vm_t* vm) {
    if (vm->nat_handler != 0) {
        vm->nat_handler(vm);
    } else {
        int a = TOS; inst_drop(vm);
        // Default handler, call from memory (not always possible)
        switch (a) {
        default: {
            void (**fp)(void) = (void (**)(void))(vm->memory + a);
            if ((*fp) != 0) (*fp)(); break; }
        }
    }
    DEBUG_VM(VM_NAT)
        }
static void inst_return(vm_t* vm) {vm->ip = TORS; vm->rp--; DEBUG_VM(VM_RETURN)}
static void inst_eq(vm_t* vm) {NOS = (NOS == TOS) ? -1 : 0;  inst_drop(vm);DEBUG_VM(VM_EQ); }
static void inst_neq(vm_t* vm) {NOS = (NOS != TOS) ? -1 : 0;  inst_drop(vm);DEBUG_VM(VM_NEQ); }
static void inst_lt(vm_t* vm) {NOS = (NOS < TOS) ? -1 : 0;  inst_drop(vm);DEBUG_VM(VM_LT); }
static void inst_gt(vm_t* vm) {NOS = (NOS > TOS) ? -1 : 0; inst_drop(vm); DEBUG_VM(VM_GT);}
static void inst_copy(vm_t* vm) {TOS = vm->address[vm->rp-TOS]; DEBUG_VM(VM_COPY);}
static void inst_paste(vm_t* vm) {
    int offset = TOS; inst_drop(vm);
    int len = TOS; inst_drop(vm);
    for(int i = 0; i < len; i++) {
        vm->address[vm->rp-(offset + i)] = vm->data[vm->sp-len+1+i];
    }
    DEBUG_VM(VM_PASTE);
}
static void inst_fetch(vm_t* vm) {
    switch (TOS) {
    case -1: TOS = vm->sp - 1; break;
    case -2: TOS = vm->rp; break;
    case -3: TOS = vm->memory_size; break;
    default: TOS = vm->memory[TOS]; break;
    }
    DEBUG_VM(VM_FETCH)
}
static void inst_store(vm_t* vm) {if(vm->memory_writable) vm->memory[TOS] = NOS; inst_drop(vm); inst_drop(vm); DEBUG_VM(VM_STORE)}
static void inst_add(vm_t* vm) {NOS += TOS; inst_drop(vm); DEBUG_VM(VM_ADD)}
static void inst_sub(vm_t* vm) {NOS -= TOS; inst_drop(vm); DEBUG_VM(VM_SUB)}
static void inst_mul(vm_t* vm) {NOS *= TOS; inst_drop(vm); DEBUG_VM(VM_MUL)}
static void inst_divmod(vm_t* vm) {
    int a, b;
    a = TOS;
    b = NOS;
    TOS = b / a;
    NOS = b % a;
    DEBUG_VM(VM_DIVMOD)
}
static void inst_and(vm_t* vm) {NOS = TOS & NOS; inst_drop(vm); DEBUG_VM(VM_AND)}
static void inst_or(vm_t* vm) {NOS = TOS | NOS; inst_drop(vm); DEBUG_VM(VM_OR)}
static void inst_xor(vm_t* vm) {NOS = TOS ^ NOS; inst_drop(vm); DEBUG_VM(VM_XOR)}
static void inst_shift(vm_t* vm) {
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
    inst_drop(vm);
    DEBUG_VM(VM_SHIFT)
}
static void inst_zret(vm_t* vm) {
    if (TOS == 0) {
        inst_drop(vm);
        vm->ip = TORS;
        vm->rp--;
    }
    DEBUG_VM(VM_ZRET)
}
static void inst_end(vm_t* vm) {vm->ip = vm->memory_size; DEBUG_VM(VM_END)}

typedef void (*Handler)(vm_t*);

Handler instructions[NUM_OPS] = {
    inst_nop, inst_lit, inst_dup, inst_drop, inst_swap, inst_push, inst_pop,
    inst_jump, inst_jnz, inst_call, inst_ccall, inst_nat, inst_return, inst_eq, inst_neq, inst_lt,
    inst_gt, inst_copy, inst_paste, inst_fetch, inst_store, inst_add, inst_sub, inst_mul, inst_divmod,
    inst_and, inst_or, inst_xor, inst_shift, inst_zret, inst_end
};

//////////////////////
/// Implementation ///
//////////////////////

void vmProcessOpcode(vm_t* vm, CELL opcode) {
    instructions[opcode](vm);
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
void vmProcessPackedOpcodes(vm_t* vm, int opcode) {
    CELL raw = opcode;
    for (int i = 0; i < 3; i++) {
        vmProcessOpcode(vm, raw & 31);
        raw = raw >> 5;
    }
}

bool vmRun(vm_t* vm, CELL from){
    CELL opcode, i, lastip;
    bool success = true;
    vm->rp++; TORS = vm->ip;
    vm->rp++; TORS = vm->memory_size; 
    vm->ip=from;
    while(vm->ip < vm->memory_size) {
        opcode = vm->memory[vm->ip];
        if(vmValidatePackedOpcodes(opcode) != 0){
            vmProcessPackedOpcodes(vm, opcode);
        } else if (opcode >= 0 && opcode < NUM_OPS) {
            vmProcessOpcode(vm, opcode);
        } else {
            success = false;
            break;
        }
        vm->ip++;
    }
    vm->ip = TORS; vm->rp--;

    return success;
}

CELL vmTOS(vm_t* vm) {return TOS;}
void vmLit(vm_t* vm, CELL c) {vm->sp++; TOS=c;}
void vmLit32(vm_t* vm, int32_t c) {vm->sp++; TOS=c>>16; vm->sp++; TOS=c&0xFFFF; }
void vmLitFloat(vm_t* vm, float f) {union {float f; int32_t i; } c; c.f = f; vmLit32(vm, c.i);}
CELL vmPop(vm_t* vm) {CELL r = TOS; if(vm->sp > 0) vm->sp--; return r;}
int32_t vmPop32(vm_t* vm) {
    int32_t ret;
    ((UCELL*)(&ret))[0] = *((UCELL*)vm->data+vm->sp);
    ((CELL*)(&ret))[1] = *((CELL*)vm->data+vm->sp-1);
    if(vm->sp > 0) vm->sp--;  if(vm->sp > 0) vm->sp--;
    return ret;
}
float vmPopFloat(vm_t* vm) {
    union {float f; int32_t i;} c;
    c.i = vmPop32(vm);
    return c.f;
}
CELL vmNOS(vm_t* vm) {return NOS;}

void vmNatHandler(vm_t* vm, void (*h)(vm_t*)) {
    vm->nat_handler = h;
}

void vmLoad(vm_t* vm, CELL* mem, CELL size, bool writable) {
    vm->memory = mem;
    vm->memory_size = size;
    vm->memory_writable = writable;
}

void vmClear(vm_t* vm) {
    memset(vm, 0, sizeof(vm_t));
    /*
    ip = sp = rp = 0;

    if(memory_writable)
        for (ip = 0; ip < memory_size; ip++)
            memory[ip] = VM_NOP;

    for (ip = 0; ip < DATA_STACK_DEPTH; ip++)
        data[ip] = 0;

    for (ip = 0; ip < CALL_STACK_DEPTH; ip++)
        address[ip] = 0;
    */
}
