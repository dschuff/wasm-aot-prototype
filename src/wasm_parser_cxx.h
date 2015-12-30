#ifndef WASM_CPP
#define WASM_CPP

#include <unordered_map>

#include "wasm.h"
#include "wasm_ast.h"

namespace wasm {

class Parser {
 public:
  Parser(const std::string& filename, bool desugar)
      : filename_(filename), desugar_(desugar) {
    (void)(desugar_);
  }
  int Parse(bool spec_script_mode) {
    scanner_ = wasm_new_scanner(filename_.c_str());
    if (!scanner_)
      return 1;
    int result = wasm_parse(scanner_, &parser_);
    result = result || parser_.errors;
    wasm_free_scanner(scanner_);
    WasmScript* script = &parser_.script;
    if (result != WASM_OK) {
      wasm_destroy_script(script);
      return result;
    }

    result = wasm_check_script(script);
    if (result != WASM_OK) {
      wasm_destroy_script(script);
      return result;
    }

    result = result || ConvertAST(*script);
    return result;
  }

  UniquePtrVector<Module> modules;
  UniquePtrVector<TestScriptExpr> test_script;

 private:
  typedef void (*ErrorCallback)(const char*);
  static void DefaultErrorCallback(const char* message) {
    fputs(message, stderr);
  }
  int ConvertAST(const WasmScript& script);

  // ErrorCallback error_callback_ = DefaultErrorCallback;
  WasmScanner scanner_;
  WasmParser parser_ = {};

  // Module* module = nullptr;
  std::string filename_;
  bool desugar_;
  // Function* current_func_ = nullptr;
  // TestScriptExpr* current_assert_return_ = nullptr;
  // Type current_type_ = Type::kVoid;
};

}  // namespace wasm
#endif  // WASM_CPP
