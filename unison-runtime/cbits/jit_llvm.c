// The JIT's only use of LLVM: a small shim over the C API (see docs/jit/implementation-plan.md, D1).
// Haskell calls these functions and nothing else from LLVM. Grown from the M0 LLVM spike (see docs/jit/progress.md, "M0 spike results").

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

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

const char *unison_jit_last_error(void) { return last_error; }

const char *unison_jit_triple(void) { return LLVMOrcLLJITGetTripleString(jit); }

// Returns 0 on success.
// Process exit while the compile thread is inside LLVM. The RTS's hs_exit
// doesn't wait for a thread in a safe foreign call, and exit() then runs
// LLVM's static destructors under the compile in progress, which crashed in
// the code generator (2026-10-04, seen at the end of transcript runs: the
// re-entry batches flush after a quiet period, which is just when a program
// finishes). So: an atexit handler, registered after LLVM's own destructors
// so that it runs before them, waits for a compile in flight to finish, and
// a compile that finishes (or starts) once we are exiting parks its thread
// instead of returning into a runtime that is gone. The process's exit then
// takes the parked thread with it.
static int compiling = 0;  // compiles in flight
static int exiting = 0;

static void park_forever(void) {
  for (;;) pause();
}

static void at_exit(void) {
  __atomic_store_n(&exiting, 1, __ATOMIC_SEQ_CST);
  struct timespec ms = {0, 1000 * 1000};
  for (int i = 0; i < 10000 && __atomic_load_n(&compiling, __ATOMIC_SEQ_CST) > 0; i++) nanosleep(&ms, NULL);
}

int unison_jit_init(void) {
  atexit(at_exit);
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
// If out_opt is non-null, it receives the module's text after the passes
// ran (malloc'd with LLVM's allocator; free it with unison_jit_free_string).
static int add_module(const char *ir, size_t len, const char *passes, char **out_opt);

int unison_jit_add_module(const char *ir, size_t len, const char *passes, char **out_opt) {
  if (__atomic_load_n(&exiting, __ATOMIC_SEQ_CST)) park_forever();
  __atomic_add_fetch(&compiling, 1, __ATOMIC_SEQ_CST);
  int r = add_module(ir, len, passes, out_opt);
  __atomic_sub_fetch(&compiling, 1, __ATOMIC_SEQ_CST);
  if (__atomic_load_n(&exiting, __ATOMIC_SEQ_CST)) park_forever();
  return r;
}

static int add_module(const char *ir, size_t len, const char *passes, char **out_opt) {
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
  if (out_opt) *out_opt = LLVMPrintModuleToString(mod);

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

void unison_jit_free_string(char *s) { LLVMDisposeMessage(s); }

// Returns the address of a compiled symbol, or 0 on failure.
// Code is generated the first time a symbol from its module is looked up.
uint64_t unison_jit_lookup(const char *name) {
  LLVMOrcExecutorAddress addr = 0;
  LLVMErrorRef e = LLVMOrcLLJITLookup(jit, &addr, name);
  if (e) {
    fail("lookup", e);
    return 0;
  }
  return addr;
}

// Makes `name` in generated code refer to `addr`. Used for runtime helpers.
int unison_jit_define_symbol(const char *name, uint64_t addr) {
  LLVMJITSymbolFlags flags = {
      LLVMJITSymbolGenericFlagsExported | LLVMJITSymbolGenericFlagsCallable, 0};
  LLVMOrcCSymbolMapPair pair = {LLVMOrcLLJITMangleAndIntern(jit, name),
                                {addr, flags}};
  LLVMErrorRef e = LLVMOrcJITDylibDefine(LLVMOrcLLJITGetMainJITDylib(jit),
                                         LLVMOrcAbsoluteSymbols(&pair, 1));
  if (e) return fail("define symbol", e);
  return 0;
}
