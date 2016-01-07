/*
 * Copyright 2016 WebAssembly Community Group participants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/* The first trap implementation attempts to avoid relying on the OS or hardware
   exceptions. Future implementations will use more tricks. */
#include <setjmp.h>
#include <stdarg.h>
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
    case kMemorySizeOverflow:
      return "runtime: memory size overflow";
    case kInvalidArgument:
      return "runtime: invalid argument";
    case kOutOfMemory:
      return "runtime: out of memory";
    case kUnknownInternalError:
      return "runtime: unknown internal error";
    default:
      return "(unknown trap)";
  }
}

void __wasm_trap(enum TrapType value) {
  /* If we don't have a trap handler installed, trap directly so we get a useful
     backtrace. */
  if (!__wasm_trap_handler_installed) {
    __wasm_report_error("Trap executed: %s\n", GetTrapName(value));
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

void __wasm_report_error(char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
}
