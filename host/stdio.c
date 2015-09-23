#include <stdio.h>

#define MODULE "stdio"
#define EXPORT_ALIAS(id, name) \
  void alias_##id() asm("." MODULE "." name) __attribute__((alias(name)))

EXPORT_ALIAS(print, "print");

static void print(int n) {
  printf("%d\n", n);
}
