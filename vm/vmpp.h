#ifndef VMPREPROCESSOR_H
#define VMPREPROCESSOR_H

#include "vm.h"

///////////////
/// HELPERS ///
///////////////

#define vmHook(name, run) vmLoad((CELL*) name ## _source, sizeof(name ## _source) / sizeof(CELL) ,false); vmRun(Sym(name, run));

#define HookCodeDef(name) name ## _prop CELL name ## _source [] = name ## _code;

///////////////////
/// DEFINITIONS ///
///////////////////

#define InitVM() defineSymbolTable(); defineImageTable(); defineNativeHook();

#define Hook(name) \
    HookCodeDef(name) void name () { vmHook(name, main); }
#define HookR(name)                                      \
    HookCodeDef(name) CELL name () { vmHook(name, main); return vmPop(); }
#define HookR1(name)                                                     \
    HookCodeDef(name) CELL name (CELL arg1) { vmLit(arg1); vmHook(name, main); return vmPop(); }
#define HookR2(name)                                                    \
    HookCodeDef(name) CELL name (CELL arg1, CELL arg2) { vmLit(arg1); vmLit(arg2); vmHook(name); return vmPop(); }
#define Hook1(name)                                                     \
    HookCodeDef(name) void name (CELL arg1) { vmLit(arg1); vmHook(name) }
#define Hook2(name)                                                     \
    HookCodeDef(name) void name (CELL arg1, CELL arg2) { vmLit(arg1); vmLit(arg2); vmHook(name) }

#define Hook_1_2(name)                                                     \
    HookCodeDef(name) int32_t name (CELL arg1) { vmLit(arg1); vmHook(name, main); return vmPop32(); }
#define Hook_2_2(name)                                                  \
    HookCodeDef(name) int32_t name (int32_t arg1) { vmPush32(arg1); vmHook(name, main); return vmPop32(); }

#define Sym(name, sym) symbolTable[ ( name ## _sym_ ## sym ) ]

//#define Mem(name, sym, type) (((type) *) name ## _source + name ## _sym_ ## sym)
//#define MemSet(name, sym, type, cell) ((type *) name ## _source + name ## _sym_ ## sym)[0] = cell
#define Callback(name, sym, fn) {void (**ptr)(void) = (void (**)(void))(name ## _source + Sym(name, sym)); *ptr = fn;}


#endif // #ifdef VMPREPROCESSOR_H
