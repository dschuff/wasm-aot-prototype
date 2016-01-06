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

    result = result || ConvertAST(*script, spec_script_mode);
    return result;
  }

  UniquePtrVector<Module> modules;
  UniquePtrVector<TestScriptExpr> test_script;

 private:
  typedef void (*ErrorCallback)(const char*);
  static void DefaultErrorCallback(const char* message) {
    fputs("Error: ", stderr);
    fputs(message, stderr);
  }
  int ConvertAST(const WasmScript& script, bool spec_script_mode);
  Module* ConvertModule(WasmModule* in_mod);
  void ConvertExprArg(WasmExpr* in_expr, Expression* out_expr);
  void ConvertExprArgVector(const WasmExprPtrVector& vec, Expression* out_expr);
  Expression* ConvertExpression(WasmExpr* in_expr);
  TestScriptExpr* ConvertInvoke(const WasmCommandInvoke& invoke);
  TestScriptExpr* ConvertTestScriptExpr(WasmCommand* command);

  ErrorCallback error_callback_ = DefaultErrorCallback;
  WasmScanner scanner_;
  WasmParser parser_ = {};

  std::string filename_;
  bool desugar_;
  Module* out_module_ = nullptr;
  Function* out_func_ = nullptr;
  WasmModule* in_module_ = nullptr;
  WasmFunc* in_func_ = nullptr;
  std::unordered_map<const WasmExport*, Export*> exports_map_;
};

}  // namespace wasm
#endif  // WASM_CPP
