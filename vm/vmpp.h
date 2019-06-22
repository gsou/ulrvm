#ifndef VMPREPROCESSOR_H
#define VMPREPROCESSOR_H

#include "vm.h"

///////////////
/// HELPERS ///
///////////////

#define vmHook(vm, name, run) vmLoad(vm, (CELL*) name ## _source, sizeof(name ## _source) / sizeof(CELL) ,false); vmRun(vm, Sym(vm, name, run));

#define HookCodeDef(name) name ## _prop CELL name ## _source [] = name ## _code;

///////////////////
/// DEFINITIONS ///
///////////////////

#define AllocVM() static defineSymbolTable(); static defineImageTable()

#define InitVM(vm) vmClear(vm); vmNatHandler(vm, nativeHook); (vm)->symbolTable = symbolTable; (vm)->imageTable = imageTable;

#define Hook(vm, name)                                           \
    HookCodeDef(name) void name () { vmHook(vm, name, main); }
#define HookR(vm, name)                                                  \
    HookCodeDef(name) CELL name () { vmHook(vm, name, main); return vmPop(vm); }
#define HookR1(vm, name)                                                 \
    HookCodeDef(name) CELL name (CELL arg1) { vmLit(vm, arg1); vmHook(vm, name, main); return vmPop(vm); }
#define HookR2(vm, name)                                                 \
    HookCodeDef(name) CELL name (CELL arg1, CELL arg2) { vmLit(vm, arg1); vmLit(arg2); vmHook(vm, name); return vmPop(vm); }
#define Hook1(vm, name)                                                  \
    HookCodeDef(name) void name (CELL arg1) { vmLit(vm, arg1); vmHook(vm, name) }
#define Hook2(vm, name)                                                  \
    HookCodeDef(name) void name (CELL arg1, CELL arg2) { vmLit(vm, arg1); vmLit(vm, arg2); vmHook(vm, name) }

#define Hook_1_2(vm, name)                                               \
    HookCodeDef(name) int32_t name (CELL arg1) { vmLit(vm, arg1); vmHook(vm, name, main); return vmPop32(vm); }
#define Hook_2_2(vm, name)                                            \
    HookCodeDef(name) int32_t name (int32_t arg1) { vmPush32(vm, arg1); vmHook(vm, name, main); return vmPop32(vm); }

#define Sym(vm, name, sym) (((vm)->symbolTable)[ ( name ## _sym_ ## sym ) ])

//#define Mem(name, sym, type) (((type) *) name ## _source + name ## _sym_ ## sym)
//#define MemSet(name, sym, type, cell) ((type *) name ## _source + name ## _sym_ ## sym)[0] = cell
#define Callback(vm, name, sym, fn) {void (**ptr)(void) = (void (**)(void))(name ## _source + Sym(vm, name, sym)); *ptr = fn;}

#ifndef VM_NAME
#define VM_NAME vm
#endif

#define pop() vmPop(VM_NAME)
#define pop32(x) vmPop32(VM_NAME)
#define lit(x) vmLit(VM_NAME, x)
#define lit32(x) vmLit32(VM_NAME, x)

#endif // #ifdef VMPREPROCESSOR_H
