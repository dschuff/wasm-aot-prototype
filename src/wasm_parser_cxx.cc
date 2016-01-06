#include "wasm_parser_cxx.h"
#include "ast_dumper.h"
#include "ast_visitor.h"
#include <cstdio>
#include <cassert>
#include <cstring>

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
                   wasm::Expression* store_val) override {
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

static UnaryOperator UnopOperator(WasmUnaryOpType opcode) {
  switch (opcode) {
    case WASM_UNARY_OP_TYPE_I32_CLZ:
    case WASM_UNARY_OP_TYPE_I64_CLZ:
      return UnaryOperator::kClz;
    case WASM_UNARY_OP_TYPE_I32_CTZ:
    case WASM_UNARY_OP_TYPE_I64_CTZ:
      return UnaryOperator::kCtz;
    case WASM_UNARY_OP_TYPE_I32_POPCNT:
    case WASM_UNARY_OP_TYPE_I64_POPCNT:
      return UnaryOperator::kPopcnt;
    case WASM_UNARY_OP_TYPE_F32_NEG:
    case WASM_UNARY_OP_TYPE_F64_NEG:
      return UnaryOperator::kNeg;
    case WASM_UNARY_OP_TYPE_F32_ABS:
    case WASM_UNARY_OP_TYPE_F64_ABS:
      return UnaryOperator::kAbs;
    case WASM_UNARY_OP_TYPE_F32_CEIL:
    case WASM_UNARY_OP_TYPE_F64_CEIL:
      return UnaryOperator::kCeil;
    case WASM_UNARY_OP_TYPE_F32_FLOOR:
    case WASM_UNARY_OP_TYPE_F64_FLOOR:
      return UnaryOperator::kFloor;
    case WASM_UNARY_OP_TYPE_F32_TRUNC:
    case WASM_UNARY_OP_TYPE_F64_TRUNC:
      return UnaryOperator::kTrunc;
    case WASM_UNARY_OP_TYPE_F32_NEAREST:
    case WASM_UNARY_OP_TYPE_F64_NEAREST:
      return UnaryOperator::kNearest;
    case WASM_UNARY_OP_TYPE_F32_SQRT:
    case WASM_UNARY_OP_TYPE_F64_SQRT:
      return UnaryOperator::kSqrt;
    default:
      assert(false && "Unexpected opcode in UnopOperator");
  }
}

static BinaryOperator BinopOperator(WasmBinaryOpType opcode) {
  switch (opcode) {
    case WASM_BINARY_OP_TYPE_I32_ADD:
    case WASM_BINARY_OP_TYPE_I64_ADD:
    case WASM_BINARY_OP_TYPE_F32_ADD:
    case WASM_BINARY_OP_TYPE_F64_ADD:
      return kAdd;
    case WASM_BINARY_OP_TYPE_I32_SUB:
    case WASM_BINARY_OP_TYPE_I64_SUB:
    case WASM_BINARY_OP_TYPE_F32_SUB:
    case WASM_BINARY_OP_TYPE_F64_SUB:
      return kSub;
    case WASM_BINARY_OP_TYPE_I32_MUL:
    case WASM_BINARY_OP_TYPE_I64_MUL:
    case WASM_BINARY_OP_TYPE_F32_MUL:
    case WASM_BINARY_OP_TYPE_F64_MUL:
      return kMul;
    case WASM_BINARY_OP_TYPE_I32_DIV_S:
    case WASM_BINARY_OP_TYPE_I64_DIV_S:
      return kDivS;
    case WASM_BINARY_OP_TYPE_I32_DIV_U:
    case WASM_BINARY_OP_TYPE_I64_DIV_U:
      return kDivU;
    case WASM_BINARY_OP_TYPE_I32_REM_S:
    case WASM_BINARY_OP_TYPE_I64_REM_S:
      return kRemS;
    case WASM_BINARY_OP_TYPE_I32_REM_U:
    case WASM_BINARY_OP_TYPE_I64_REM_U:
      return kRemU;
    case WASM_BINARY_OP_TYPE_I32_AND:
    case WASM_BINARY_OP_TYPE_I64_AND:
      return kAnd;
    case WASM_BINARY_OP_TYPE_I32_OR:
    case WASM_BINARY_OP_TYPE_I64_OR:
      return kOr;
    case WASM_BINARY_OP_TYPE_I32_XOR:
    case WASM_BINARY_OP_TYPE_I64_XOR:
      return kXor;
    case WASM_BINARY_OP_TYPE_I32_SHL:
    case WASM_BINARY_OP_TYPE_I64_SHL:
      return kShl;
    case WASM_BINARY_OP_TYPE_I32_SHR_U:
    case WASM_BINARY_OP_TYPE_I64_SHR_U:
      return kShrU;
    case WASM_BINARY_OP_TYPE_I32_SHR_S:
    case WASM_BINARY_OP_TYPE_I64_SHR_S:
      return kShrS;
    case WASM_BINARY_OP_TYPE_F32_DIV:
    case WASM_BINARY_OP_TYPE_F64_DIV:
      return kDiv;
    case WASM_BINARY_OP_TYPE_F32_COPYSIGN:
    case WASM_BINARY_OP_TYPE_F64_COPYSIGN:
      return kCopySign;
    case WASM_BINARY_OP_TYPE_F32_MIN:
    case WASM_BINARY_OP_TYPE_F64_MIN:
      return kMin;
    case WASM_BINARY_OP_TYPE_F32_MAX:
    case WASM_BINARY_OP_TYPE_F64_MAX:
      return kMax;
    default:
      assert(false && "Unexpected opcode in BinopOperator");
  }
}

static CompareOperator CmpOperator(WasmCompareOpType opcode) {
  switch (opcode) {
    case WASM_COMPARE_OP_TYPE_I32_EQ:
    case WASM_COMPARE_OP_TYPE_I64_EQ:
    case WASM_COMPARE_OP_TYPE_F32_EQ:
    case WASM_COMPARE_OP_TYPE_F64_EQ:
      return kEq;
    case WASM_COMPARE_OP_TYPE_I32_NE:
    case WASM_COMPARE_OP_TYPE_I64_NE:
    case WASM_COMPARE_OP_TYPE_F32_NE:
    case WASM_COMPARE_OP_TYPE_F64_NE:
      return kNE;
    case WASM_COMPARE_OP_TYPE_I32_LT_S:
    case WASM_COMPARE_OP_TYPE_I64_LT_S:
      return kLtS;
    case WASM_COMPARE_OP_TYPE_I32_LE_S:
    case WASM_COMPARE_OP_TYPE_I64_LE_S:
      return kLeS;
    case WASM_COMPARE_OP_TYPE_I32_LT_U:
    case WASM_COMPARE_OP_TYPE_I64_LT_U:
      return kLtU;
    case WASM_COMPARE_OP_TYPE_I32_LE_U:
    case WASM_COMPARE_OP_TYPE_I64_LE_U:
      return kLeU;
    case WASM_COMPARE_OP_TYPE_I32_GT_S:
    case WASM_COMPARE_OP_TYPE_I64_GT_S:
      return kGtS;
    case WASM_COMPARE_OP_TYPE_I32_GT_U:
    case WASM_COMPARE_OP_TYPE_I64_GT_U:
      return kGtU;
    case WASM_COMPARE_OP_TYPE_I32_GE_S:
    case WASM_COMPARE_OP_TYPE_I64_GE_S:
      return kGeS;
    case WASM_COMPARE_OP_TYPE_I32_GE_U:
    case WASM_COMPARE_OP_TYPE_I64_GE_U:
      return kGeU;
    case WASM_COMPARE_OP_TYPE_F32_LT:
    case WASM_COMPARE_OP_TYPE_F64_LT:
      return kLt;
    case WASM_COMPARE_OP_TYPE_F32_LE:
    case WASM_COMPARE_OP_TYPE_F64_LE:
      return kLe;
    case WASM_COMPARE_OP_TYPE_F32_GT:
    case WASM_COMPARE_OP_TYPE_F64_GT:
      return kGt;
    case WASM_COMPARE_OP_TYPE_F32_GE:
    case WASM_COMPARE_OP_TYPE_F64_GE:
      return kGe;
    default:
      assert(false && "Unexpected opcode in CmpOperator");
  }
}

static ConversionOperator ConvOperator(WasmConvertOpType opcode) {
  switch (opcode) {
    case WASM_CONVERT_OP_TYPE_I32_TRUNC_S_F32:
    case WASM_CONVERT_OP_TYPE_I64_TRUNC_S_F32:
      return kTruncSFloat32;
    case WASM_CONVERT_OP_TYPE_I32_TRUNC_S_F64:
    case WASM_CONVERT_OP_TYPE_I64_TRUNC_S_F64:
      return kTruncSFloat64;
    case WASM_CONVERT_OP_TYPE_I32_TRUNC_U_F32:
    case WASM_CONVERT_OP_TYPE_I64_TRUNC_U_F32:
      return kTruncUFloat32;
    case WASM_CONVERT_OP_TYPE_I32_TRUNC_U_F64:
    case WASM_CONVERT_OP_TYPE_I64_TRUNC_U_F64:
      return kTruncUFloat64;
    case WASM_CONVERT_OP_TYPE_I32_WRAP_I64:
      return kWrapInt64;
    case WASM_CONVERT_OP_TYPE_I64_EXTEND_S_I32:
      return kExtendSInt32;
    case WASM_CONVERT_OP_TYPE_I64_EXTEND_U_I32:
      return kExtendUInt32;
    case WASM_CONVERT_OP_TYPE_F32_CONVERT_S_I32:
    case WASM_CONVERT_OP_TYPE_F64_CONVERT_S_I32:
      return kConvertSInt32;
    case WASM_CONVERT_OP_TYPE_F32_CONVERT_U_I32:
    case WASM_CONVERT_OP_TYPE_F64_CONVERT_U_I32:
      return kConvertUInt32;
    case WASM_CONVERT_OP_TYPE_F32_CONVERT_S_I64:
    case WASM_CONVERT_OP_TYPE_F64_CONVERT_S_I64:
      return kConvertSInt64;
    case WASM_CONVERT_OP_TYPE_F32_CONVERT_U_I64:
    case WASM_CONVERT_OP_TYPE_F64_CONVERT_U_I64:
      return kConvertUInt64;
    case WASM_CONVERT_OP_TYPE_F32_DEMOTE_F64:
      return kDemoteFloat64;
    case WASM_CONVERT_OP_TYPE_F64_PROMOTE_F32:
      return kPromoteFloat32;
    default:
      assert(false && "Unexpected opcode in ConvOperator");
  }
}

static ConversionOperator CastOperator(WasmCastOpType opcode) {
  switch (opcode) {
    case WASM_CAST_OP_TYPE_F32_REINTERPRET_I32:
    case WASM_CAST_OP_TYPE_F64_REINTERPRET_I64:
      return kReinterpretInt;
    case WASM_CAST_OP_TYPE_I32_REINTERPRET_F32:
    case WASM_CAST_OP_TYPE_I64_REINTERPRET_F64:
      return kReinterpretFloat;
    default:
      assert(false && "Unexpected opcode in CastOperator");
  }
}

static Expression* ConvertExpression(const WasmExpr& in_expr,
                                     const WasmFunc& in_func,
                                     const Function& out_func) {
  switch (in_expr.type) {
    case WASM_EXPR_TYPE_CONST: {
      auto* out_expr = new ConstantExpression(in_expr.const_.type);
      switch (out_expr->expr_type) {
        case WASM_TYPE_I32:
          out_expr->literal.value.i32 = in_expr.const_.u32;
          return out_expr;
        case WASM_TYPE_I64:
          out_expr->literal.value.i64 = in_expr.const_.u64;
          return out_expr;
        case WASM_TYPE_F32:
          out_expr->literal.value.f32 = in_expr.const_.f32;
          return out_expr;
        case WASM_TYPE_F64:
          out_expr->literal.value.f64 = in_expr.const_.f64;
          return out_expr;
        default:
          assert(false);
      }
    }
    case WASM_EXPR_TYPE_NOP:
      return new Expression(Expression::kNop, Type::kVoid);
    case WASM_EXPR_TYPE_BLOCK: {
      auto* out_expr = new Expression(Expression::kBlock, Type::kVoid);
      for (size_t i = 0; i < in_expr.block.exprs.size; ++i) {
        out_expr->exprs.emplace_back(
            ConvertExpression(*in_expr.block.exprs.data[i], in_func, out_func));
      }
      if (out_expr->exprs.size())
        out_expr->expr_type = out_expr->exprs.back()->expr_type;
      return out_expr;
    }
    case WASM_EXPR_TYPE_RETURN: {
      auto* out_expr = new Expression(Expression::kReturn, Type::kAny);
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.return_.expr, in_func, out_func));
      return out_expr;
    }
    case WASM_EXPR_TYPE_GET_LOCAL: {
      int local_index =
          wasm_get_local_index_by_var(&in_func, &in_expr.get_local.var);
      return LocalExpression::GetGetLocal(out_func.locals[local_index].get());
    }
    case WASM_EXPR_TYPE_SET_LOCAL: {
      int local_index =
          wasm_get_local_index_by_var(&in_func, &in_expr.set_local.var);
      auto* out_expr =
          LocalExpression::GetSetLocal(out_func.locals[local_index].get());
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.set_local.expr, in_func, out_func));
      return out_expr;
    }
    case WASM_EXPR_TYPE_UNARY: {
      auto* out_expr = new UnaryExpression(
          UnopOperator(in_expr.unary.op.op_type), in_expr.unary.op.type);
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.unary.expr, in_func, out_func));
      return out_expr;
    }
    case WASM_EXPR_TYPE_BINARY: {
      auto* out_expr = new BinaryExpression(
          BinopOperator(in_expr.binary.op.op_type), in_expr.binary.op.type);
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.binary.left, in_func, out_func));
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.binary.right, in_func, out_func));
      return out_expr;
    }
    case WASM_EXPR_TYPE_COMPARE: {
      auto* out_expr = new CompareExpression(
          in_expr.compare.op.type, CmpOperator(in_expr.compare.op.op_type));
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.compare.left, in_func, out_func));
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.compare.right, in_func, out_func));
      return out_expr;
    }
    case WASM_EXPR_TYPE_CONVERT: {
      auto* out_expr = new ConversionExpression(
          ConvOperator(in_expr.convert.op.op_type), in_expr.convert.op.type2,
          in_expr.convert.op.type);
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.convert.expr, in_func, out_func));
      return out_expr;
    }
    case WASM_EXPR_TYPE_CAST: {
      auto* out_expr =
          new ConversionExpression(CastOperator(in_expr.cast.op.op_type),
                                   in_expr.cast.op.type2, in_expr.cast.op.type);
      out_expr->exprs.emplace_back(
          ConvertExpression(*in_expr.cast.expr, in_func, out_func));
      return out_expr;
    }
    default:
      assert(false && "Unhandled expression type");
      return nullptr;
  }
}

static void ConvertFunction(const WasmFunc& in_func, Function* out_func) {
  for (size_t i = 0; i < in_func.exprs.size; ++i) {
    out_func->body.emplace_back(
        ConvertExpression(*in_func.exprs.data[i], in_func, *out_func));
  }
}

static Module* ConvertModule(const WasmModule& in_mod) {
  std::unique_ptr<Module> out_mod(new Module());
  // First set up module-level constructs: segments, functions, imports, exports
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
      Variable* out_var = out_func->locals.back().get();
      out_var->index = j;
      if (j < in_func->params.types.size)
        out_func->args.push_back(out_var);
    }
    for (size_t j = 0; j < in_func->params_and_locals.bindings.size; ++j) {
      const WasmBinding& binding = in_func->params_and_locals.bindings.data[j];
      // TODO: location
      assert(binding.index < static_cast<int>(out_func->locals.size()));
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

  // Convert the functions
  for (size_t i = 0; i < in_mod.funcs.size; ++i) {
    ConvertFunction(*in_mod.funcs.data[i], out_mod->functions[i].get());
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
