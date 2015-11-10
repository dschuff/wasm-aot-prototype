#include <stdio.h>

#define MODULE "stdio"

#if defined(__linux__)
#define PREFIX "."
#elif defined( __APPLE__)
#define PREFIX "_."
#else
#error "Unsupported platform"
#endif

#define EXPORT(retty, id, name, ...)                 \
  retty id(__VA_ARGS__) asm(PREFIX MODULE "." name); \
  retty id(__VA_ARGS__)

EXPORT(void, print, "print", int n) {
  printf("%d\n", n);
}
