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
