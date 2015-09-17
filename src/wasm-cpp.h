#ifndef WASM_CPP
#define WASM_CPP

#include "wasm.h"
#include "wasm-ast.h"
#include "wasm-parse.h"

#include <string>
#include <unordered_map>
#include <cassert>

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
    parser.before_function = before_function;
    parser.after_function = unimplemented<WasmModule*, WasmFunction*, int>;
    parser.before_export = unimplemented<WasmModule*>;
    parser.after_export = after_export;
    parser.before_binary = unimplemented<enum WasmOpcode>;
    parser.before_block = before_block;
    parser.after_block = after_block;
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
    parser.after_nop = after_nop;
    parser.before_return = unimplemented<>;
    parser.before_set_local = unimplemented<int>;
    parser.before_store = unimplemented<enum WasmOpcode, uint8_t>;
    parser.before_store_global = unimplemented<int>;
    parser.before_unary = unimplemented<enum WasmOpcode>;
  }
  void Parse() {
    wasm_parse_file(&parser, &tokenizer_);
  }

  Module module;

 protected:
  virtual void Unimplemented(const char* name);
  virtual void AfterNop();
  virtual WasmParserCookie BeforeBlock();
  virtual void AfterBlock(WasmParserCookie);
  virtual void BeforeFunction(WasmModule* m, WasmFunction* f);
  virtual void AfterFunction(WasmModule* m, WasmFunction* f);
  virtual void BeforeModule(WasmModule* m);
  virtual void AfterExport(WasmModule* m, WasmExport* e);
 private:
  WasmParser parser = {};
  WasmSource source_;
  WasmTokenizer tokenizer_;

  std::unordered_map<WasmFunction*, Function*> functions_;

  std::vector<std::unique_ptr<Expression>>* insertion_point_;
  void insert(Expression* ex) {
    assert(insertion_point_);
    insertion_point_->emplace_back(ex);
  }


  static void after_nop(void* user) {
    static_cast<Parser*>(user)->AfterNop();
  }

  static WasmParserCookie before_block(void* user) {
    return static_cast<Parser*>(user)->BeforeBlock();
  }

  static void after_block(int, WasmParserCookie cookie, void* user) {
    static_cast<Parser*>(user)->AfterBlock(cookie);
  }

  static void before_function(WasmModule* m, WasmFunction* f,
                              void* user) {
    static_cast<Parser*>(user)->BeforeFunction(m, f);
  }
  static void after_function(WasmModule* m, WasmFunction* f,
                             void* user) {
    static_cast<Parser*>(user)->AfterFunction(m, f);
  }

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
