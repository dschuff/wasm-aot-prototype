#include <string.h>
#include "wart_trap.h"

/* For now, only support having one running module at a time. We expect the
   generated spec script code to instantiate the module by calling
   __wasm_allocate_memory and tear it down with __wasm_free_memory.
*/
const size_t kPageSize =
    64 * 1024;  // NaCl uses 64k, it should work everywhere.

size_t __wasm_page_size() {
  return kPageSize;
}

void __wasm_init_memory(void* base, size_t initial_size) {
  /* For now, do nothing. */
}

void __wasm_grow_memory(size_t delta) {
  if ((delta & (kPageSize - 1)) != 0) {
    __wasm_report_error(
        "grow_memory delta %zd is not a multiple of page size %zd\n",
        delta,
        kPageSize);
    __wasm_trap(kInvalidArgument);
  }
}

void __wasm_fini_memory(void* base) {
  /* For now, do nothing. */
}

void __wasm_init_segment(void* base, size_t address, size_t size, void* src) {
  memcpy((char*)base + address, src, size);
}
