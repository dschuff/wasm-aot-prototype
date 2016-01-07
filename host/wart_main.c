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

#include <stdio.h>
#include <stdint.h>

typedef void(InitFunction)(void);
extern InitFunction* __wasm_init_array[];

typedef void(FiniFunction)(void);
extern FiniFunction* __wasm_fini_array[];

int32_t __wasm_exit_status = 0;

int main(int argc, char** argv) {
  /* Call module init constructors */
  InitFunction** ini = __wasm_init_array;
  while (*ini)
    (*ini++)();

  /* Eventually we should have a way to specify the entry point for modules,
     but for now, just expect that any necessary asserts are in the array
     along with the constructors. So when we get here we've already called
     all of the user code. */
  /* Now call the destructors */
  FiniFunction** fini = __wasm_fini_array;
  while (*fini)
    (*fini++)();
  return __wasm_exit_status;
}
