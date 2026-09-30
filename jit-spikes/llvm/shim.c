// M0 spike: the smallest C shim over LLVM's C API that the JIT needs (decision D1).
// Haskell calls these six functions and nothing else from LLVM.

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <llvm-c/Core.h>
#include <llvm-c/Error.h>
#include <llvm-c/IRReader.h>
#include <llvm-c/LLJIT.h>
#include <llvm-c/Orc.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Transforms/PassBuilder.h>

static LLVMOrcLLJITRef jit = NULL;
static LLVMTargetMachineRef machine = NULL;
static char last_error[4096] = "";

static int fail(const char *where, LLVMErrorRef e) {
  char *msg = LLVMGetErrorMessage(e);
  snprintf(last_error, sizeof last_error, "%s: %s", where, msg);
  LLVMDisposeErrorMessage(msg);
  return -1;
}

const char *jit_last_error(void) { return last_error; }

const char *jit_triple(void) { return LLVMOrcLLJITGetTripleString(jit); }

// Returns 0 on success.
int jit_init(void) {
  if (jit) return 0;
  LLVMInitializeNativeTarget();
  LLVMInitializeNativeAsmPrinter();
  LLVMInitializeNativeAsmParser();

  LLVMErrorRef e = LLVMOrcCreateLLJIT(&jit, NULL);
  if (e) return fail("create LLJIT", e);

  // A target machine of our own, used only to run the optimization passes.
  char *triple = LLVMGetDefaultTargetTriple();
  char *cpu = LLVMGetHostCPUName();
  char *features = LLVMGetHostCPUFeatures();
  LLVMTargetRef target;
  char *msg = NULL;
  if (LLVMGetTargetFromTriple(triple, &target, &msg)) {
    snprintf(last_error, sizeof last_error, "target: %s", msg);
    LLVMDisposeMessage(msg);
    return -1;
  }
  machine = LLVMCreateTargetMachine(target, triple, cpu, features,
                                    LLVMCodeGenLevelDefault, LLVMRelocDefault,
                                    LLVMCodeModelJITDefault);
  LLVMDisposeMessage(triple);
  LLVMDisposeMessage(cpu);
  LLVMDisposeMessage(features);
  return 0;
}

// Parses a module from IR text, optimizes it, and hands it to the JIT.
// `passes` is a pass pipeline such as "default<O2>", or "" for none.
int jit_add_module(const char *ir, size_t len, const char *passes) {
  LLVMContextRef ctx = LLVMContextCreate();
  LLVMOrcThreadSafeContextRef tsctx =
      LLVMOrcCreateNewThreadSafeContextFromLLVMContext(ctx);

  LLVMMemoryBufferRef buf =
      LLVMCreateMemoryBufferWithMemoryRangeCopy(ir, len, "unison-jit");
  LLVMModuleRef mod;
  char *msg = NULL;
  if (LLVMParseIRInContext(ctx, buf, &mod, &msg)) {
    snprintf(last_error, sizeof last_error, "parse: %s", msg);
    LLVMDisposeMessage(msg);
    LLVMOrcDisposeThreadSafeContext(tsctx);
    return -1;
  }

  if (passes && passes[0]) {
    LLVMPassBuilderOptionsRef opts = LLVMCreatePassBuilderOptions();
    LLVMErrorRef e = LLVMRunPasses(mod, passes, machine, opts);
    LLVMDisposePassBuilderOptions(opts);
    if (e) {
      LLVMDisposeModule(mod);
      LLVMOrcDisposeThreadSafeContext(tsctx);
      return fail("passes", e);
    }
  }

  LLVMOrcThreadSafeModuleRef tsm = LLVMOrcCreateNewThreadSafeModule(mod, tsctx);
  LLVMOrcDisposeThreadSafeContext(tsctx);
  LLVMErrorRef e = LLVMOrcLLJITAddLLVMIRModule(
      jit, LLVMOrcLLJITGetMainJITDylib(jit), tsm);
  if (e) {
    LLVMOrcDisposeThreadSafeModule(tsm);
    return fail("add module", e);
  }
  return 0;
}

// Returns the address of a compiled symbol, or 0 on failure.
// Code is generated the first time a symbol from its module is looked up.
uint64_t jit_lookup(const char *name) {
  LLVMOrcExecutorAddress addr = 0;
  LLVMErrorRef e = LLVMOrcLLJITLookup(jit, &addr, name);
  if (e) {
    fail("lookup", e);
    return 0;
  }
  return addr;
}

// Makes `name` in generated code refer to `addr`. Used for runtime helpers.
int jit_define_symbol(const char *name, uint64_t addr) {
  LLVMJITSymbolFlags flags = {
      LLVMJITSymbolGenericFlagsExported | LLVMJITSymbolGenericFlagsCallable, 0};
  LLVMOrcCSymbolMapPair pair = {LLVMOrcLLJITMangleAndIntern(jit, name),
                                {addr, flags}};
  LLVMErrorRef e = LLVMOrcJITDylibDefine(LLVMOrcLLJITGetMainJITDylib(jit),
                                         LLVMOrcAbsoluteSymbols(&pair, 1));
  if (e) return fail("define symbol", e);
  return 0;
}

// A stand-in for a runtime helper written in C.
int64_t spike_helper(int64_t x) { return x * 2 + 1; }
uint64_t spike_helper_addr(void) { return (uint64_t)&spike_helper; }
