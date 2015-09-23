// If there's no main() in the link, this one will get pulled in. It will try to
// call an export _start from a module named "main"
#ifndef MAIN_MODULE
#define MAIN_MODULE "main"
#endif
#ifndef ENTRY_FUNCTION
#define ENTRY_FUNCTION "_start"
#endif
#define ENTRY_EXPORT "." MAIN_MODULE "." ENTRY_FUNCTION

void entry_export() asm(ENTRY_EXPORT);
int main(int argc, char** argv) {
  entry_export();
  return 0;
}
