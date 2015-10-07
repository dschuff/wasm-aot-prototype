#include "wasm_parser_cxx.h"
#include "ast_dumper.h"
#include "ast_visitor.h"
#include <cstdio>
#include <cassert>

namespace {
// Despite its name, this class not only checks expected types, but it sets them
// from the top down (which is not done during parsing). The expectations are
// neede for some code generation.
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
  void VisitCall(wasm::Expression* expr,
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
  void VisitSetLocal(wasm::Expression* expr,
                     wasm::Variable* var,
                     wasm::Expression* value) override {
    value->expected_type = var->type;
    VisitExpression(value);
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
      fprintf(stderr, "Type mismatch: expected %d, actual %d\n",
              (WasmType)expected, (WasmType)actual);
    }
  }
  const wasm::Function* current_function_ = nullptr;
};
}

namespace wasm {

static_assert(sizeof(WasmParserCookie) == sizeof(void*),
              "WasmParserCookie size does not match pointer size");

void Parser::error(WasmSourceLocation loc, const char* msg) {
  fprintf(stderr, "%s:%d:%d: %s", loc.source->filename, loc.line, loc.col, msg);
}

void Parser::after_nop() {
  auto* expr = new Expression(WASM_OPCODE_NOP);
  expr->expr_type = Type::kVoid;  // Should collapse with kAny?
  Insert(expr);
}

WasmParserCookie Parser::before_block() {
  auto* expr = new Expression(WASM_OPCODE_BLOCK);
  expr->expr_type = current_type_;
  // TODO:This is ugly. Is block the only thing with an unknown number of exprs?
  InsertAndPush(expr, kUnknownExpectedExprs);
  return reinterpret_cast<WasmParserCookie>(expr);
}

void Parser::after_block(WasmType ty, int num_exprs, WasmParserCookie cookie) {
  PopInsertionPoint();
  Expression* block_expr = reinterpret_cast<Expression*>(cookie);
  assert(block_expr->opcode == WASM_OPCODE_BLOCK);
  block_expr->expr_type = ty;
}

WasmParserCookie Parser::before_if() {
  auto* expr = new Expression(WASM_OPCODE_IF);
  expr->expr_type = current_type_;
  InsertAndPush(expr, kUnknownExpectedExprs);
  return reinterpret_cast<WasmParserCookie>(expr);
}

void Parser::after_if(WasmType ty, int with_else, WasmParserCookie cookie) {
  PopInsertionPoint();
  Expression* if_expr = reinterpret_cast<Expression*>(cookie);
  assert(if_expr->opcode == WASM_OPCODE_IF);
  if_expr->expr_type = ty;
  assert(if_expr->exprs.size() == (unsigned)with_else + 2);
}

void Parser::ParseCall(bool is_import, int index) {
  auto* expr = new Expression(WASM_OPCODE_CALL);
  expr->callee_index = index;
  expr->is_import = is_import;
  assert(is_import ? module->imports.size() > static_cast<unsigned>(index)
                   : module->functions.size() > static_cast<unsigned>(index));
  if (is_import) {
    expr->callee = module->imports[index].get();
  } else {
    expr->callee = module->functions[index].get();
  }
  expr->expr_type = expr->callee->result_type;
  InsertAndPush(expr, expr->callee->args.size());
}

void Parser::before_call(int func_index) {
  ParseCall(false, func_index);
}

void Parser::before_call_import(int import_index) {
  ParseCall(true, import_index);
}

void Parser::before_return() {
  auto* expr = new Expression(WASM_OPCODE_RETURN);
  expr->expr_type = Type::kAny;
  InsertAndPush(expr, kUnknownExpectedExprs);
}

void Parser::after_return(WasmType ty) {
  PopInsertionPoint();
}

void Parser::after_get_local(int index) {
  auto* expr = new Expression(WASM_OPCODE_GET_LOCAL);
  assert(current_func_);
  expr->local_var = current_func_->locals[index].get();
  expr->expr_type = expr->local_var->type;
  Insert(expr);
}

void Parser::before_set_local(int index) {
  auto* expr = new Expression(WASM_OPCODE_SET_LOCAL);
  assert(current_func_);
  expr->local_var = current_func_->locals[index].get();
  expr->expr_type = expr->local_var->type;
  InsertAndPush(expr, 1);
}

void Parser::after_const(WasmOpcode opcode, WasmType ty, WasmNumber value) {
  auto* expr = new Expression(opcode);
  expr->expr_type = ty;
  expr->literal.type = ty;
  switch (ty) {
    case WASM_TYPE_I32:
      assert(opcode == WASM_OPCODE_I32_CONST || opcode == WASM_OPCODE_I8_CONST);
      expr->literal.value.i32 = value.i32;
      break;
    case WASM_TYPE_I64:
      assert(opcode == WASM_OPCODE_I64_CONST);
      expr->literal.value.i64 = value.i64;
      break;
    case WASM_TYPE_F32:
      assert(opcode == WASM_OPCODE_F32_CONST);
      expr->literal.value.f32 = value.f32;
      break;
    case WASM_TYPE_F64:
      assert(opcode == WASM_OPCODE_F64_CONST);
      expr->literal.value.f64 = value.f64;
      break;
    default:
      assert(false);
  }
  Insert(expr);
}

void Parser::before_compare(WasmOpcode opcode) {
  auto* expr = new Expression(opcode);
  Type op_type = Type::kVoid;
  switch (opcode) {
    case WASM_OPCODE_I32_EQ:
    case WASM_OPCODE_I32_NE:
    case WASM_OPCODE_I32_SLT:
    case WASM_OPCODE_I32_SLE:
    case WASM_OPCODE_I32_ULT:
    case WASM_OPCODE_I32_ULE:
    case WASM_OPCODE_I32_SGT:
    case WASM_OPCODE_I32_UGT:
    case WASM_OPCODE_I32_SGE:
    case WASM_OPCODE_I32_UGE:
      op_type = Type::kI32;
      break;
    case WASM_OPCODE_I64_EQ:
    case WASM_OPCODE_I64_NE:
    case WASM_OPCODE_I64_SLT:
    case WASM_OPCODE_I64_SLE:
    case WASM_OPCODE_I64_ULT:
    case WASM_OPCODE_I64_ULE:
    case WASM_OPCODE_I64_SGT:
    case WASM_OPCODE_I64_UGT:
    case WASM_OPCODE_I64_SGE:
    case WASM_OPCODE_I64_UGE:
      op_type = Type::kI64;
      break;
    case WASM_OPCODE_F32_EQ:
    case WASM_OPCODE_F32_NE:
    case WASM_OPCODE_F32_LT:
    case WASM_OPCODE_F32_LE:
    case WASM_OPCODE_F32_GT:
    case WASM_OPCODE_F32_GE:
      op_type = Type::kF32;
      break;
    case WASM_OPCODE_F64_EQ:
    case WASM_OPCODE_F64_NE:
    case WASM_OPCODE_F64_LT:
    case WASM_OPCODE_F64_LE:
    case WASM_OPCODE_F64_GT:
    case WASM_OPCODE_F64_GE:
      op_type = Type::kF64;
      break;
    default:
      assert(false && "Unexpected opcode in before_compare");
  }
  CompareOperator relop;
  switch (opcode) {
    case WASM_OPCODE_I32_EQ:
    case WASM_OPCODE_I64_EQ:
    case WASM_OPCODE_F32_EQ:
    case WASM_OPCODE_F64_EQ:
      relop = kEq;
      break;
    case WASM_OPCODE_I32_NE:
    case WASM_OPCODE_I64_NE:
    case WASM_OPCODE_F32_NE:
    case WASM_OPCODE_F64_NE:
      relop = kNE;
      break;
    case WASM_OPCODE_I32_SLT:
    case WASM_OPCODE_I64_SLT:
      relop = kLtS;
      break;
    case WASM_OPCODE_I32_SLE:
    case WASM_OPCODE_I64_SLE:
      relop = kLeS;
      break;
    case WASM_OPCODE_I32_ULT:
    case WASM_OPCODE_I64_ULT:
      relop = kLtU;
      break;
    case WASM_OPCODE_I32_ULE:
    case WASM_OPCODE_I64_ULE:
      relop = kLeU;
      break;
    case WASM_OPCODE_I32_SGT:
    case WASM_OPCODE_I64_SGT:
      relop = kGtS;
      break;
    case WASM_OPCODE_I32_UGT:
    case WASM_OPCODE_I64_UGT:
      relop = kGtU;
      break;
    case WASM_OPCODE_I32_SGE:
    case WASM_OPCODE_I64_SGE:
      relop = kGeS;
      break;
    case WASM_OPCODE_I32_UGE:
    case WASM_OPCODE_I64_UGE:
      relop = kGeU;
      break;
    case WASM_OPCODE_F32_LT:
    case WASM_OPCODE_F64_LT:
      relop = kLt;
      break;
    case WASM_OPCODE_F32_LE:
    case WASM_OPCODE_F64_LE:
      relop = kLe;
      break;
    case WASM_OPCODE_F32_GT:
    case WASM_OPCODE_F64_GT:
      relop = kGt;
      break;
    case WASM_OPCODE_F32_GE:
    case WASM_OPCODE_F64_GE:
      relop = kGe;
      break;
    default:
      assert(false && "Unexpected opcode in before_compare");
  }
  expr->expr_type = Type::kI32;
  expr->compare_type = op_type;
  expr->relop = relop;
  InsertAndPush(expr, 2);
}

void Parser::before_function(WasmModule* m, WasmFunction* f) {
  current_func_ = functions_[f];
  current_type_ = current_func_->result_type;
  ResetInsertionPoint(&current_func_->body, kUnknownExpectedExprs);
}

void Parser::after_function(WasmModule* m, WasmFunction* f, int num_exprs) {
  /* TODO: move this to a separate pass/prepass?*/
  // Desugar from a top-level list of exprs to an implicit block expr
  if (desugar_) {
    Function* func = functions_[f];
    if (func->body.size() > 1) {
      auto* expr = new Expression(WASM_OPCODE_BLOCK);
      std::swap(expr->exprs, func->body);
      assert(insertion_points_.size() == 1);
      insertion_points_[0].point = &func->body;
      Insert(expr);
    } else if (func->body.empty()) {
      Insert(new Expression(WASM_OPCODE_NOP));
    }
  }
  PopInsertionPoint();
  current_func_ = nullptr;
}

void Parser::before_module(WasmModule* m) {
  assert(!module);
  modules.emplace_back(new Module());
  exports_by_name_.clear();
  module = modules.back().get();
  module->initial_memory_size = m->initial_memory_size;
  module->max_memory_size = m->max_memory_size;
  assert(module->max_memory_size >= module->initial_memory_size);

  module->functions.reserve(m->functions.size);
  for (size_t i = 0; i < m->functions.size; ++i) {
    WasmFunction* parser_func = &m->functions.data[i];
    module->functions.emplace_back(new Function(parser_func->result_type, i));
    Function* func = module->functions.back().get();
    functions_[parser_func] = func;

    func->locals.reserve(parser_func->locals.size);
    func->args.reserve(parser_func->num_args);
    for (size_t j = 0; j < parser_func->locals.size; ++j) {
      const WasmVariable& var = parser_func->locals.data[j];
      func->locals.emplace_back(new Variable(var.type));
      func->locals.back()->index = j;
      if (static_cast<int>(j) < parser_func->num_args)
        func->args.push_back(func->locals.back().get());
    }

    for (size_t j = 0; j < parser_func->local_bindings.size; ++j) {
      const WasmBinding& binding = parser_func->local_bindings.data[j];
      func->locals[binding.index]->local_name.assign(binding.name);
    }
  }

  for (size_t i = 0; i < m->function_bindings.size; ++i) {
    const WasmBinding& binding = m->function_bindings.data[i];
    module->functions[binding.index]->local_name.assign(binding.name);
  }

  for (size_t i = 0; i < m->imports.size; ++i) {
    const WasmImport& parser_import = m->imports.data[i];
    module->imports.emplace_back(new Import(parser_import.result_type,
                                            parser_import.module_name,
                                            parser_import.func_name));
    Import* imp = module->imports.back().get();
    for (size_t j = 0; j < parser_import.args.size; ++j) {
      imp->locals.emplace_back(new Variable(parser_import.args.data[j].type));
      imp->args.push_back(imp->locals.back().get());
    }
  }
  for (size_t i = 0; i < m->import_bindings.size; ++i) {
    const WasmBinding& binding = m->import_bindings.data[i];
    Import* imp = module->imports[binding.index].get();
    imp->local_name.assign(binding.name);
  }

  if (m->segments.size) {
    assert(module->initial_memory_size > 0);
    module->segments.reserve(m->segments.size);
  }
  for (size_t i = 0; i < m->segments.size; ++i) {
    const WasmSegment& parser_seg = m->segments.data[i];
    module->segments.emplace_back(
        new Segment(parser_seg.size, parser_seg.address));
    Segment* seg = module->segments.back().get();
    seg->initial_data.resize(seg->size);
    wasm_copy_segment_data(parser_seg.data, &seg->initial_data[0], seg->size);
  }
}

void Parser::after_module(WasmModule* m) {
  TypeChecker checker = {};
  checker.Visit(*module);
  module = nullptr;
}

void Parser::after_export(WasmModule* m,
                          WasmFunction* f,
                          const char* export_name) {
  Function* func = functions_[f];
  assert(export_name);
  module->exports.emplace_back(new Export(func, export_name, module));
  exports_by_name_.emplace(std::string(export_name),
                           module->exports.back().get());
}

WasmParserCookie Parser::before_invoke(const char* invoke_name,
                                       int invoke_function_index) {
  assert(modules.size());
  Module* last_module = modules.back().get();
  auto* expr = new TestScriptExpr(last_module, TestScriptExpr::kInvoke);
  if (current_assert_eq_) {
    current_assert_eq_->invoke.reset(expr);
  } else {
    test_script.emplace_back(expr);
  }
  assert(exports_by_name_.count(std::string(invoke_name)));
  expr->callee = exports_by_name_.find(std::string(invoke_name))->second;
  assert(last_module->functions[invoke_function_index].get() ==
         expr->callee->function);
  PushInsertionPoint(&expr->exprs, expr->callee->function->args.size());
  return reinterpret_cast<WasmParserCookie>(expr);
}

void Parser::after_invoke(WasmParserCookie cookie) {
  TypeChecker checker = {};
  checker.Visit(reinterpret_cast<TestScriptExpr*>(cookie));
}

WasmParserCookie Parser::before_assert_eq() {
  assert(modules.size() && !module);
  Module* last_module = modules.back().get();
  test_script.emplace_back(
      new TestScriptExpr(last_module, TestScriptExpr::kAssertEq));
  current_assert_eq_ = test_script.back().get();
  ResetInsertionPoint(&current_assert_eq_->exprs, 1);
  return reinterpret_cast<WasmParserCookie>(test_script.back().get());
}

void Parser::after_assert_eq(WasmType ty, WasmParserCookie cookie) {
  auto* expr = reinterpret_cast<TestScriptExpr*>(cookie);
  // The parser has already checked the types. We just need to propagate the
  // expectations down to the expectation expr tree.
  expr->type = ty;
  expr->invoke->type = ty;
  expr->exprs.front()->expected_type = ty;
  TypeChecker checker = {};
  checker.VisitExpression(expr->exprs.front().get());
  checker.Visit(expr);
}

}  // namespace wasm
