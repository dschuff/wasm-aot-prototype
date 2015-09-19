#ifndef WASM_CPP
#define WASM_CPP

#include "wasm.h"
#include "wasm-parse.h"
#include "wasm_ast.h"

#include <string>
#include <unordered_map>
#include <cassert>
#include <cstring>

namespace wasm {

#define EACH_CALLBACK0                     \
  CALLBACK(after_nop, void)                \
  CALLBACK(before_block, WasmParserCookie) \
  CALLBACK(before_return, void)

#define EACH_CALLBACK1                    \
  CALLBACK(before_call, void, int)        \
  CALLBACK(before_call_import, void, int) \
  CALLBACK(before_module, void, WasmModule*)

#define EACH_CALLBACK2                                        \
  CALLBACK(error, void, WasmSourceLocation, const char*)      \
  CALLBACK(after_block, void, int, WasmParserCookie)          \
  CALLBACK(before_function, void, WasmModule*, WasmFunction*) \
  CALLBACK(after_export, void, WasmModule*, WasmExport*)

#define EACH_CALLBACK3                                          \
  CALLBACK(after_const, void, WasmOpcode, WasmType, WasmNumber) \
  CALLBACK(after_function, void, WasmModule*, WasmFunction*, int)

class Parser {
 public:
  Parser(const char* start, const char* end, const std::string& filename,
         bool desugar)
      : desugar_(desugar) {
    source_.filename = filename.c_str();
    source_.start = start;
    source_.end = end;
    wasm_init_parser(&parser, &source_);
    parser.user_data = this;

#define CALLBACK(name, retty, ...) parser.name = wrapper_##name;
    EACH_CALLBACK0
    EACH_CALLBACK1
    EACH_CALLBACK2
    EACH_CALLBACK3
#undef CALLBACK
  }
  int Parse() { return wasm_parse_module(&parser); }

  Module module;

 private:
#define CALLBACK(name, retty, ...) retty name(__VA_ARGS__);
  EACH_CALLBACK0
  EACH_CALLBACK1
  EACH_CALLBACK2
  EACH_CALLBACK3
#undef CALLBACK

  WasmParser parser = {};
  WasmSource source_;
  bool desugar_;

  std::unordered_map<WasmFunction*, Function*> functions_;

  std::vector<std::unique_ptr<Expression>>* insertion_point_;
  void insert(Expression* ex) {
    assert(insertion_point_);
    insertion_point_->emplace_back(ex);
  }
  void insert_update(Expression* ex) {
    insert(ex);
    insertion_point_ = &ex->exprs;
  }

// The callbacks unfortunately are split by arity because we need to
// interleave the va-args (which are the arg types) with arg names, and
// variadic macros don't have a way to do that.
#define CALLBACK(name, retty)                  \
  static retty wrapper_##name(void* user) {    \
    return static_cast<Parser*>(user)->name(); \
  }
  EACH_CALLBACK0
#undef CALLBACK

#define CALLBACK(name, retty, arg1ty)                    \
  static retty wrapper_##name(arg1ty arg1, void* user) { \
    return static_cast<Parser*>(user)->name(arg1);       \
  }
  EACH_CALLBACK1
#undef CALLBACK

#define CALLBACK(name, retty, arg1ty, arg2ty)                         \
  static retty wrapper_##name(arg1ty arg1, arg2ty arg2, void* user) { \
    return static_cast<Parser*>(user)->name(arg1, arg2);              \
  }
  EACH_CALLBACK2
#undef CALLBACK

#define CALLBACK(name, retty, arg1ty, arg2ty, arg3ty)                \
  static retty wrapper_##name(arg1ty arg1, arg2ty arg2, arg3ty arg3, \
                              void* user) {                          \
    return static_cast<Parser*>(user)->name(arg1, arg2, arg3);       \
  }
  EACH_CALLBACK3
#undef CALLBACK
};

}  // namespace wasm
#endif  // WASM_CPP
