#include "wasm_parser_cxx.h"
#include "ast_dumper.h"
#include "ast_visitor.h"
#include <cstdio>
#include <cassert>

namespace {
// Despite its name, this class does not do much checking of expected types,
// (the parser does type checking already) but it sets them from the top down
// (which is not done during parsing). The expectations are neede for some code
// generation.
class TypeChecker : public wasm::AstVisitor<void, void> {
 public:
  void VisitExpression(wasm::Expression* expr) override {
    AstVisitor::VisitExpression(expr);
  }

 protected:
  void VisitFunction(const wasm::Function& f) override {
    current_function_ = &f;
    for (auto& expr : f.body)
      expr->expected_type = wasm::Type::kVoid;
    if (!f.body.empty())
      f.body.back()->expected_type = f.result_type;
    AstVisitor::VisitFunction(f);
  }
  void VisitBlock(wasm::Expression* expr,
                  wasm::UniquePtrVector<wasm::Expression>* exprs) override {
    auto& back = exprs->back();
    for (auto& e : *exprs) {
      if (e == back) {
        e->expected_type = expr->expected_type;
      } else {
        e->expected_type = wasm::Type::kVoid;
      }
      VisitExpression(e.get());
    }
  }
  void VisitIf(wasm::Expression* expr,
               wasm::Expression* condition,
               wasm::Expression* then,
               wasm::Expression* els) override {
    // TODO: explicitly convert the condition result to i32
    condition->expected_type = wasm::Type::kI32;
    VisitExpression(condition);
    then->expected_type = expr->expected_type;
    VisitExpression(then);
    if (els) {
      els->expected_type = expr->expected_type;
      VisitExpression(els);
    }
  }
  void VisitArgs(wasm::Callable* callee,
                 wasm::UniquePtrVector<wasm::Expression>* args) {
    int i = 0;
    for (auto& arg : *args) {
      assert(arg->expected_type == wasm::Type::kUnknown ||
             arg->expected_type == callee->args[i]->type);
      arg->expected_type = callee->args[i]->type;
      CheckType(arg->expected_type, callee->args[i]->type);
      ++i;
      VisitExpression(arg.get());
    }
  }
  void VisitCall(wasm::CallExpression* expr,
                 bool is_import,
                 wasm::Callable* callee,
                 int callee_index,
                 wasm::UniquePtrVector<wasm::Expression>* args) override {
    VisitArgs(callee, args);
  }
  void VisitReturn(wasm::Expression* expr,
                   wasm::UniquePtrVector<wasm::Expression>* value) override {
    if (value->size()) {
      value->back()->expected_type = current_function_->result_type;
      VisitExpression(value->back().get());
    }
  }
  void VisitSetLocal(wasm::LocalExpression* expr,
                     wasm::Variable* var,
                     wasm::Expression* value) override {
    value->expected_type = var->type;
    VisitExpression(value);
  }
  void VisitMemory(wasm::Expression* expr,
                   wasm::MemoryOperator memop,
                   wasm::MemType mem_type,
                   uint32_t mem_alignment,
                   uint64_t mem_offset,
                   bool is_signed,
                   wasm::Expression* address,
                   wasm::Expression* store_val) {
    address->expected_type = wasm::Type::kI32;  // TODO: wasm64
    VisitExpression(address);
    if (store_val) {
      store_val->expected_type = expr->expr_type;
      VisitExpression(store_val);
    }
  }
  void VisitUnop(wasm::Expression* expr,
                 wasm::UnaryOperator unop,
                 wasm::Expression* operand) override {
    operand->expected_type = expr->expr_type;
    VisitExpression(operand);
  }
  void VisitBinop(wasm::Expression* expr,
                  wasm::BinaryOperator binop,
                  wasm::Expression* lhs,
                  wasm::Expression* rhs) override {
    lhs->expected_type = expr->expr_type;
    VisitExpression(lhs);
    rhs->expected_type = expr->expr_type;
    VisitExpression(lhs);
  }
  void VisitCompare(wasm::Expression* expr,
                    wasm::Type compare_type,
                    wasm::CompareOperator relop,
                    wasm::Expression* lhs,
                    wasm::Expression* rhs) override {
    lhs->expected_type = compare_type;
    VisitExpression(lhs);
    rhs->expected_type = compare_type;
    VisitExpression(rhs);
  }
  void VisitConversion(wasm::ConversionExpression* expr,
                       wasm::ConversionOperator cvt,
                       wasm::Expression* operand) override {
    operand->expected_type = expr->operand_type;
    VisitExpression(operand);
  }
  void VisitInvoke(wasm::TestScriptExpr* expr,
                   wasm::Export* callee,
                   wasm::UniquePtrVector<wasm::Expression>* args) override {
    VisitArgs(callee->function, args);
  }

 private:
  static void CheckType(wasm::Type expected, wasm::Type actual) {
    assert(expected != wasm::Type::kUnknown);
    assert(actual != wasm::Type::kUnknown);
    if (expected != wasm::Type::kVoid && actual != wasm::Type::kAny &&
        actual != expected) {
      fprintf(stderr,
              "Type mismatch: expected %d, actual %d\n",
              (WasmType)expected,
              (WasmType)actual);
    }
  }
  const wasm::Function* current_function_ = nullptr;
};
}

namespace wasm {

static Module* ConvertModule(const WasmModule& in_mod) {
  std::unique_ptr<Module> out_mod(new Module());
  // Memory segment declarations and initializers
  if (in_mod.memory) {
    out_mod->initial_memory_size = in_mod.memory->initial_size;
    out_mod->max_memory_size = in_mod.memory->max_size;
    if (in_mod.memory->segments.size) {
      for (size_t i = 0; i < in_mod.memory->segments.size; ++i) {
        const WasmSegment* in_seg = &in_mod.memory->segments.data[i];
        out_mod->segments.emplace_back(new Segment(in_seg->size, in_seg->addr));
        Segment* out_seg = out_mod->segments.back().get();
        out_seg->initial_data.resize(out_seg->size);
        memcpy(out_seg->initial_data.data(), in_seg->data, in_seg->size);
      }
    }
  }
  // Functions, with locals/params
  std::unordered_map<std::string, Function*> functions_by_name;
  out_mod->functions.reserve(in_mod.funcs.size);
  for (size_t i = 0; i < in_mod.funcs.size; ++i) {
    const WasmFunc* in_func = in_mod.funcs.data[i];
    out_mod->functions.emplace_back(
        new Function(in_func->result_type, in_func->name, i));
    Function* out_func = out_mod->functions.back().get();
    if (in_func->name.length) {
      auto result = functions_by_name.emplace(out_func->local_name, out_func);
      assert(result.second);
    }

    assert(in_func->locals.types.size + in_func->params.types.size ==
           in_func->params_and_locals.types.size);
    out_func->locals.reserve(in_func->params_and_locals.types.size);
    out_func->args.reserve(in_func->params.types.size);
    for (size_t j = 0; j < in_func->params_and_locals.types.size; ++j) {
      out_func->locals.emplace_back(
          new Variable(in_func->params_and_locals.types.data[j]));
      out_func->locals.back()->index = j;
      if (j < in_func->params.types.size)
        out_func->args.push_back(out_func->locals.back().get());
    }
    for (size_t j = 0; j < in_func->params_and_locals.bindings.size; ++j) {
      const WasmBinding& binding = in_func->params_and_locals.bindings.data[j];
      // TODO: location
      assert(binding.index < out_func->locals.size());
      out_func->locals[binding.index]->local_name.assign(binding.name.start,
                                                         binding.name.length);
    }
  }
  // Imports, with signatures
  for (size_t i = 0; i < in_mod.imports.size; ++i) {
    const WasmImport* in_import = in_mod.imports.data[i];
    assert(in_import->import_type == WASM_IMPORT_HAS_FUNC_SIGNATURE);
    out_mod->imports.emplace_back(new Import(in_import->func_sig.result_type,
                                             in_import->name,
                                             in_import->module_name,
                                             in_import->func_name));
    Import* imp = out_mod->imports.back().get();
    for (size_t j = 0; j < in_import->func_sig.param_types.size; ++j) {
      imp->locals.emplace_back(
          new Variable(in_import->func_sig.param_types.data[j]));
      imp->args.push_back(imp->locals.back().get());
    }
  }
  // Exports
  for (size_t i = 0; i < in_mod.exports.size; ++i) {
    const WasmExport* in_export = in_mod.exports.data[i];
    Function* func;
    if (in_export->var.type == WASM_VAR_TYPE_INDEX) {
      func = out_mod->functions[in_export->var.index].get();
    } else {
      std::string local_name(in_export->var.name.start,
                             in_export->var.name.length);
      auto it = functions_by_name.find(local_name);
      assert(it != functions_by_name.end());
      func = it->second;
    }
    out_mod->exports.emplace_back(
        new Export(func, in_export->name, out_mod.get()));
  }
  return out_mod.release();
}

int Parser::ConvertAST(const WasmScript& script) {
  for (size_t i = 0; i < script.commands.size; ++i) {
    WasmCommand* command = &script.commands.data[i];
    if (command->type == WASM_COMMAND_TYPE_MODULE) {
      modules.emplace_back(ConvertModule(command->module));
    }
  }
  return 0;
}

}  // namespace wasm
