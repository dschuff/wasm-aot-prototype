/*
 * Copyright 2016 WebAssembly Community Group participants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
  void ConvertExprArg(WasmExpr* in_expr,
                      Expression* out_expr,
                      Type expected_type);
  void ConvertBlockArgs(const WasmExprPtrVector& in_vec,
                        UniquePtrVector<Expression>* out_vec,
                        Type expected_type);
  Expression* ConvertExpression(WasmExpr* in_expr, Type expected_type);
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
