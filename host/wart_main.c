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
