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

#include <stdlib.h>

#include "wart_trap.h"

/* For now, only support having one running module at a time. We expect the
   generated spec script code to instantiate the module by calling
   __wasm_allocate_memory and tear it down with __wasm_free_memory.
*/
static unsigned char* __wasm_linear_memory_base = NULL;
const size_t kPageSize =
    16 * 1024;  // NaCl uses 16k, it should work everywhere.

size_t __wasm_page_size() {
  return kPageSize;
}

void __wasm_allocate_memory(size_t initial_size) {
  int ret = posix_memalign(&__wasm_linear_memory_base, kPageSize, initial_size);
  if (ret == ENOMEM) {
    __wasm_report_error("Out of memory allocating heap of %zd bytes\n",
                        initial_size);
    __wasm_trap(kOutOfMemory);
  }
  if (ret != 0) {
    __wasm_report_error(
        "Error allocating heap of %zd bytes with aligment %zd\n",
        initial_size,
        kPageSize);
    __wasm_trap(kUnknownInternalError);
  }
}

void __wasm_grow_memory(size_t delta) {
  if (delta & (kPageSize - 1) != 0) {
    __wasm_report_error(
        "grow_memory delta %zd is not a multiple of page size %zd\n",
        delta,
        kPageSize);
    __wasm_trap(kInvalidArgument);
  }
}

void __wasm_free_memory() {
  free(__wasm_linear_memory_base);
}
