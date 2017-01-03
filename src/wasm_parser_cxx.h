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

// WABT includes
#include "ast.h"
#include "ast-parser.h"
#include "validator.h"

// WAOT includes
#include "wasm_ast.h"

namespace wasm {

class Parser {
 public:
  Parser(const std::string& filename, bool desugar)
      : filename_(filename), desugar_(desugar) {
    (void)(desugar_);
    allocator_ = wasm_get_libc_allocator();
  }
  int Parse(bool spec_script_mode) {
    int ret = 1;
    WasmAstLexer* lexer =
        wasm_new_ast_file_lexer(wasm_get_libc_allocator(), filename_.c_str());
    if (!lexer)
      return 1;
    WasmSourceErrorHandler error_handler = WASM_SOURCE_ERROR_HANDLER_DEFAULT;
    WasmScript script;
    WasmResult result = wasm_parse_ast(lexer, &script, &error_handler);

    if (WASM_SUCCEEDED(result)) {
      result = wasm_validate_script(wasm_get_libc_allocator(), lexer, &script,
                                    &error_handler);
    }
    // For now, don't support assert_{invalid,malformed}
    if (WASM_SUCCEEDED(result)) {
      ret = ConvertAST(script, spec_script_mode);
    }
    wasm_destroy_ast_lexer(lexer);
    wasm_destroy_script(&script);
    wasm_destroy_allocator(wasm_get_libc_allocator());
    return ret;
  }

  UniquePtrVector<Module> modules;
  UniquePtrVector<TestScriptExpr> test_script;

 private:
  int ConvertAST(const WasmScript& script, bool spec_script_mode);
  Module* ConvertModule(WasmModule* in_mod);
  void ConvertExprArg(WasmExpr* in_expr,
                      Expression* out_expr,
                      Type expected_type);
  void ConvertBlockArgs(const WasmExpr* in_vec,
                        UniquePtrVector<Expression>* out_vec,
                        Type expected_type);
  Expression* ConvertExpression(const WasmExpr* in_expr, Type expected_type);
  TestScriptExpr* ConvertInvoke(const WasmAction& invoke);
  TestScriptExpr* ConvertTestScriptExpr(WasmCommand* command);

  WasmAllocator* allocator_;
  std::string filename_;
  bool desugar_;
  Module* out_module_ = nullptr;
  Function* out_func_ = nullptr;
  WasmModule* in_module_ = nullptr;
  WasmFunc* in_func_ = nullptr;
  std::unordered_map<const WasmExport*, Export*> exports_map_;
  std::vector<Expression*> expr_stack_;
  Expression* Push(Expression* e) { expr_stack_.push_back(e); return e; }
  Expression* Pop() {
    assert(expr_stack_.size());
    Expression* e = expr_stack_.back();
    expr_stack_.pop_back();
    return e;
  }
};

}  // namespace wasm
#endif  // WASM_CPP
