#ifndef WASM_CPP
#define WASM_CPP

#include "wasm.h"
#include "wasm-ast.h"
#include "wasm-parse.h"

#include <string>

namespace wasm {

class Parser {
public:
  Parser(const char* start, const char* end) {
    source_.start = start;
    source_.end = end;
    tokenizer_.source = source_;
    tokenizer_.loc.pos = source_.start;
    tokenizer_.loc.line = 1;
    tokenizer_.loc.col = 1;

    parser.user_data = this;
    parser.before_module = before_module;
    parser.after_module = unimplemented<WasmModule*>;
    parser.before_function = unimplemented<WasmModule*, WasmFunction*>;
    parser.after_function = unimplemented<WasmModule*, WasmFunction*, int>;
    parser.before_export = unimplemented<WasmModule*>;
    parser.after_export = after_export;
    parser.before_binary = unimplemented<enum WasmOpcode>;
    parser.before_block = unimplementedT<WasmParserCookie>;
    parser.after_block = unimplemented<int, WasmParserCookie>;
    parser.after_break = unimplemented<int>;
    parser.before_call = unimplemented<int>;
    parser.before_compare = unimplemented<enum WasmOpcode>;
    parser.after_const = unimplemented<enum WasmOpcode, WasmType, WasmNumber>;
    parser.before_convert = unimplemented<enum WasmOpcode>;
    parser.before_label = unimplementedT<WasmParserCookie>;
    parser.after_label = unimplemented<int, WasmParserCookie>;
    parser.after_get_local = unimplemented<int>;
    parser.before_loop = unimplementedT<WasmParserCookie>;
    parser.after_loop = unimplemented<int, WasmParserCookie>;
    parser.before_if = unimplementedT<WasmParserCookie>;
    parser.after_if = unimplemented<int, WasmParserCookie>;
    parser.before_load = unimplemented<enum WasmOpcode, uint8_t>;
    parser.after_load_global = unimplemented<int>;
    parser.after_nop = unimplemented<>;
    parser.before_return = unimplemented<>;
    parser.before_set_local = unimplemented<int>;
    parser.before_store = unimplemented<enum WasmOpcode, uint8_t>;
    parser.before_store_global = unimplemented<int>;
    parser.before_unary = unimplemented<enum WasmOpcode>;
  }
  void Parse() {
    wasm_parse_file(&parser, &tokenizer_);
  }

  WasmAst::Module module;

 protected:
  virtual void Unimplemented(const char* name);
  virtual void BeforeModule(WasmModule* m);
  virtual void AfterExport(WasmModule* m, WasmExport* e);
 private:
  WasmParser parser = {};
  WasmSource source_;
  WasmTokenizer tokenizer_;

  static void before_module(WasmModule* m, void* user) {
    static_cast<Parser*>(user)->BeforeModule(m);
  }

  static void after_export(WasmModule* m, WasmExport* e, void* user) {
    static_cast<Parser*>(user)->AfterExport(m, e);
  }
  template <typename T, typename... Args> static T
  unimplementedT(Args... args, void *user) {
    unimplemented<Args...>(args..., user);
    return T();
  }

  template <typename... Args> static void
  unimplemented(Args... args, void *user) {

  }
};

} // namespace wasm
#endif // WASM_CPP
