/* The first trap implementation attempts to avoid relying on the OS or hardware
   exceptions. Future implementations will use more tricks. */
#include <setjmp.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef DEBUG_TRAPS
/* This is a GNU extension, but useful for debugging */
#include <execinfo.h>
#endif

#include "wart_trap.h"

/* Defined in assert.c */
void __wasm_assert_trap_fail(int32_t assert_num);

static jmp_buf __wasm_trap_env;
static int __wasm_trap_handler_installed = 0;

static const char* GetTrapName(enum TrapType type) {
  switch (type) {
    case kIntegerOverflow:
      return "runtime: integer overflow";
    case kIntegerDivideByZero:
      return "runtime: integer divide by zero";
    case kInvalidConversionToInteger:
      return "runtime: invalid conversion to integer";
    case kMemoryBounds:
      return "runtime: out of bounds memory access";
    case kMemoryAddress:
      return "runtime: illegal address value";
    default:
      return "(unknown trap)";
  }
}

void __wasm_trap(int value) {
  /* If we don't have a trap handler installed, trap directly so we get a useful
     backtrace. */
  if (!__wasm_trap_handler_installed) {
    fprintf(stderr, "Trap executed: %s\n", GetTrapName(value));
#ifdef DEBUG_TRAPS
    void* bt_buf[4096];
    backtrace_symbols_fd(bt_buf, backtrace(bt_buf, 4096), 2);
#else
    abort();
#endif
  }
  longjmp(__wasm_trap_env, value);
}

/* TODO: Make this thread safe?. Or just assume assert_traps can't run in
   in parallel, which is probably more sane. */
typedef void (*invoke_fn)(void);
void __wasm_assert_trap(int32_t assert_num, invoke_fn fn) {
  __wasm_trap_handler_installed = 1;
  int setjmp_ret = setjmp(__wasm_trap_env);
  if (!setjmp_ret) {
    // run the invoke
    fn();
    // If the invoke returns, we didn't get a trap.
    __wasm_assert_trap_fail(assert_num);
  }
  __wasm_trap_handler_installed = 0;
}
