#ifndef TRAP_H
#define TRAP_H

/* This file is included from C runtime code and C++ compiler code. */
#ifdef __cplusplus
namespace wart {
#endif

enum TrapType {
  // Reserve value 0 so this can be returned from setjmp
  kIntegerOverflow = 1,
  kIntegerDivideByZero,
  kInvalidConversionToInteger,
  kMemoryBounds,
  kMemorySizeOverflow,
  kInvalidArgument,
  // Internal errors
  kOutOfMemory,
  kUnknownInternalError,
};

void __wasm_trap(enum TrapType trap_type);
void __wasm_report_error(char* fmt, ...);

#ifdef __cplusplus
}  // namespace wart
#endif

#endif
