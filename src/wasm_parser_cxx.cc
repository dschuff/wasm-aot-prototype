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
  void VisitConversion(wasm::Expression* expr,
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

void Parser::error(const char* msg) {
  WasmSourceLocation loc = current_callback_info_->loc;
  fprintf(stderr, "%s:%d:%d: %s", loc.source->filename, loc.line, loc.col, msg);
}

void Parser::after_nop() {
  auto* expr = new Expression(Expression::kNop);
  expr->expr_type = Type::kVoid;  // Should collapse with kAny?
  Insert(expr);
}

void Parser::before_block(int with_label) {
  auto* expr = new Expression(Expression::kBlock);
  expr->expr_type = current_type_;
  // TODO:This is ugly. Is block the only thing with an unknown number of exprs?
  InsertAndPush(expr, kUnknownExpectedExprs);
  SetCookie<Expression*>(expr);
}

void Parser::after_block(WasmType ty, int num_exprs) {
  PopInsertionPoint();
  Expression* block_expr = GetCookie<Expression*>();
  assert(block_expr->kind == Expression::kBlock);
  block_expr->expr_type = ty;
}

void Parser::before_if() {
  auto* expr = new Expression(Expression::kIf);
  expr->expr_type = current_type_;
  InsertAndPush(expr, kUnknownExpectedExprs);
  SetCookie<Expression*>(expr);
}

void Parser::after_if(WasmType ty, int with_else) {
  PopInsertionPoint();
  Expression* if_expr = GetCookie<Expression*>();
  assert(if_expr->kind == Expression::kIf);
  if_expr->expr_type = ty;
  assert(if_expr->exprs.size() == (unsigned)with_else + 2);
}

void Parser::ParseCall(bool is_import, int index) {
  auto* expr = new Expression(Expression::kCallDirect);
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
  auto* expr = new Expression(Expression::kReturn);
  expr->expr_type = Type::kAny;
  InsertAndPush(expr, kUnknownExpectedExprs);
}

void Parser::after_return(WasmType ty) {
  PopInsertionPoint();
}

void Parser::after_get_local(int index) {
  auto* expr = new Expression(Expression::kGetLocal);
  assert(current_func_);
  expr->local_var = current_func_->locals[index].get();
  expr->expr_type = expr->local_var->type;
  Insert(expr);
}

void Parser::before_set_local(int index) {
  auto* expr = new Expression(Expression::kSetLocal);
  assert(current_func_);
  expr->local_var = current_func_->locals[index].get();
  expr->expr_type = expr->local_var->type;
  InsertAndPush(expr, 1);
}

void Parser::after_const(WasmOpcode opcode, WasmType ty, WasmNumber value) {
  auto* expr = new Expression(Expression::kConst);
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

static Type UnopType(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_CLZ:
    case WASM_OPCODE_I32_CTZ:
    case WASM_OPCODE_I32_POPCNT:
      return Type::kI32;
    case WASM_OPCODE_I64_CLZ:
    case WASM_OPCODE_I64_CTZ:
    case WASM_OPCODE_I64_POPCNT:
      return Type::kI64;
    case WASM_OPCODE_F32_NEG:
    case WASM_OPCODE_F32_ABS:
    case WASM_OPCODE_F32_CEIL:
    case WASM_OPCODE_F32_FLOOR:
    case WASM_OPCODE_F32_TRUNC:
    case WASM_OPCODE_F32_NEAREST:
    case WASM_OPCODE_F32_SQRT:
      return Type::kF32;
    case WASM_OPCODE_F64_NEG:
    case WASM_OPCODE_F64_ABS:
    case WASM_OPCODE_F64_CEIL:
    case WASM_OPCODE_F64_FLOOR:
    case WASM_OPCODE_F64_TRUNC:
    case WASM_OPCODE_F64_NEAREST:
    case WASM_OPCODE_F64_SQRT:
      return Type::kF64;
    default:
      fprintf(stderr, "opcode %x\n", opcode);
      assert(false && "Unexpected opcode in UnopType");
  }
}

static UnaryOperator UnopOperator(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_CLZ:
    case WASM_OPCODE_I64_CLZ:
      return UnaryOperator::kClz;
    case WASM_OPCODE_I32_CTZ:
    case WASM_OPCODE_I64_CTZ:
      return UnaryOperator::kCtz;
    case WASM_OPCODE_I32_POPCNT:
    case WASM_OPCODE_I64_POPCNT:
      return UnaryOperator::kPopcnt;
    case WASM_OPCODE_F32_NEG:
    case WASM_OPCODE_F64_NEG:
      return UnaryOperator::kNeg;
    case WASM_OPCODE_F32_ABS:
    case WASM_OPCODE_F64_ABS:
      return UnaryOperator::kAbs;
    case WASM_OPCODE_F32_CEIL:
    case WASM_OPCODE_F64_CEIL:
      return UnaryOperator::kCeil;
    case WASM_OPCODE_F32_FLOOR:
    case WASM_OPCODE_F64_FLOOR:
      return UnaryOperator::kFloor;
    case WASM_OPCODE_F32_TRUNC:
    case WASM_OPCODE_F64_TRUNC:
      return UnaryOperator::kTrunc;
    case WASM_OPCODE_F32_NEAREST:
    case WASM_OPCODE_F64_NEAREST:
      return UnaryOperator::kNearest;
    case WASM_OPCODE_F32_SQRT:
    case WASM_OPCODE_F64_SQRT:
      return UnaryOperator::kSqrt;
    default:
      assert(false && "Unexpected opcode in UnopOperator");
  }
}

void Parser::before_unary(WasmOpcode opcode) {
  auto* expr = new Expression(Expression::kUnary);
  expr->expr_type = UnopType(opcode);
  expr->unop = UnopOperator(opcode);
  InsertAndPush(expr, 1);
}

static Type BinopType(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_ADD:
    case WASM_OPCODE_I32_SUB:
    case WASM_OPCODE_I32_MUL:
    case WASM_OPCODE_I32_SDIV:
    case WASM_OPCODE_I32_UDIV:
    case WASM_OPCODE_I32_SREM:
    case WASM_OPCODE_I32_UREM:
    case WASM_OPCODE_I32_AND:
    case WASM_OPCODE_I32_OR:
    case WASM_OPCODE_I32_XOR:
    case WASM_OPCODE_I32_SHL:
    case WASM_OPCODE_I32_SHR:
    case WASM_OPCODE_I32_SAR:
      return Type::kI32;
    case WASM_OPCODE_I64_ADD:
    case WASM_OPCODE_I64_SUB:
    case WASM_OPCODE_I64_MUL:
    case WASM_OPCODE_I64_SDIV:
    case WASM_OPCODE_I64_UDIV:
    case WASM_OPCODE_I64_SREM:
    case WASM_OPCODE_I64_UREM:
    case WASM_OPCODE_I64_AND:
    case WASM_OPCODE_I64_OR:
    case WASM_OPCODE_I64_XOR:
    case WASM_OPCODE_I64_SHL:
    case WASM_OPCODE_I64_SHR:
    case WASM_OPCODE_I64_SAR:
      return Type::kI64;
    case WASM_OPCODE_F32_ADD:
    case WASM_OPCODE_F32_SUB:
    case WASM_OPCODE_F32_MUL:
    case WASM_OPCODE_F32_DIV:
    case WASM_OPCODE_F32_COPYSIGN:
    case WASM_OPCODE_F32_MIN:
    case WASM_OPCODE_F32_MAX:
      return Type::kF32;
    case WASM_OPCODE_F64_ADD:
    case WASM_OPCODE_F64_SUB:
    case WASM_OPCODE_F64_MUL:
    case WASM_OPCODE_F64_DIV:
    case WASM_OPCODE_F64_COPYSIGN:
    case WASM_OPCODE_F64_MIN:
    case WASM_OPCODE_F64_MAX:
      return Type::kF64;
    default:
      assert(false && "Unexpected opcode in BinopType");
  }
}

static BinaryOperator BinopOperator(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_ADD:
    case WASM_OPCODE_I64_ADD:
    case WASM_OPCODE_F32_ADD:
    case WASM_OPCODE_F64_ADD:
      return kAdd;
    case WASM_OPCODE_I32_SUB:
    case WASM_OPCODE_I64_SUB:
    case WASM_OPCODE_F32_SUB:
    case WASM_OPCODE_F64_SUB:
      return kSub;
    case WASM_OPCODE_I32_MUL:
    case WASM_OPCODE_I64_MUL:
    case WASM_OPCODE_F32_MUL:
    case WASM_OPCODE_F64_MUL:
      return kMul;
    case WASM_OPCODE_I32_SDIV:
    case WASM_OPCODE_I64_SDIV:
      return kDivS;
    case WASM_OPCODE_I32_UDIV:
    case WASM_OPCODE_I64_UDIV:
      return kDivU;
    case WASM_OPCODE_I32_SREM:
    case WASM_OPCODE_I64_SREM:
      return kRemS;
    case WASM_OPCODE_I32_UREM:
    case WASM_OPCODE_I64_UREM:
      return kRemU;
    case WASM_OPCODE_I32_AND:
    case WASM_OPCODE_I64_AND:
      return kAnd;
    case WASM_OPCODE_I32_OR:
    case WASM_OPCODE_I64_OR:
      return kOr;
    case WASM_OPCODE_I32_XOR:
    case WASM_OPCODE_I64_XOR:
      return kXor;
    case WASM_OPCODE_I32_SHL:
    case WASM_OPCODE_I64_SHL:
      return kShl;
    case WASM_OPCODE_I32_SHR:
    case WASM_OPCODE_I64_SHR:
      return kShrU;
    case WASM_OPCODE_I32_SAR:
    case WASM_OPCODE_I64_SAR:
      return kShrS;
    case WASM_OPCODE_F32_DIV:
    case WASM_OPCODE_F64_DIV:
      return kDiv;
    case WASM_OPCODE_F32_COPYSIGN:
    case WASM_OPCODE_F64_COPYSIGN:
      return kCopySign;
    case WASM_OPCODE_F32_MIN:
    case WASM_OPCODE_F64_MIN:
      return kMin;
    case WASM_OPCODE_F32_MAX:
    case WASM_OPCODE_F64_MAX:
      return kMax;
    default:
      assert(false && "Unexpected opcode in BinopOperator");
  }
}

void Parser::before_binary(WasmOpcode opcode) {
  auto* expr = new Expression(Expression::kBinary);
  expr->expr_type = BinopType(opcode);
  expr->binop = BinopOperator(opcode);
  InsertAndPush(expr, 2);
}

static Type CompareType(WasmOpcode opcode) {
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
      return Type::kI32;
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
      return Type::kI64;
    case WASM_OPCODE_F32_EQ:
    case WASM_OPCODE_F32_NE:
    case WASM_OPCODE_F32_LT:
    case WASM_OPCODE_F32_LE:
    case WASM_OPCODE_F32_GT:
    case WASM_OPCODE_F32_GE:
      return Type::kF32;
    case WASM_OPCODE_F64_EQ:
    case WASM_OPCODE_F64_NE:
    case WASM_OPCODE_F64_LT:
    case WASM_OPCODE_F64_LE:
    case WASM_OPCODE_F64_GT:
    case WASM_OPCODE_F64_GE:
      return Type::kF64;
    default:
      assert(false && "Unexpected opcode in CompareType");
  }
}

static CompareOperator CmpOperator(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_EQ:
    case WASM_OPCODE_I64_EQ:
    case WASM_OPCODE_F32_EQ:
    case WASM_OPCODE_F64_EQ:
      return kEq;
    case WASM_OPCODE_I32_NE:
    case WASM_OPCODE_I64_NE:
    case WASM_OPCODE_F32_NE:
    case WASM_OPCODE_F64_NE:
      return kNE;
    case WASM_OPCODE_I32_SLT:
    case WASM_OPCODE_I64_SLT:
      return kLtS;
    case WASM_OPCODE_I32_SLE:
    case WASM_OPCODE_I64_SLE:
      return kLeS;
    case WASM_OPCODE_I32_ULT:
    case WASM_OPCODE_I64_ULT:
      return kLtU;
    case WASM_OPCODE_I32_ULE:
    case WASM_OPCODE_I64_ULE:
      return kLeU;
    case WASM_OPCODE_I32_SGT:
    case WASM_OPCODE_I64_SGT:
      return kGtS;
    case WASM_OPCODE_I32_UGT:
    case WASM_OPCODE_I64_UGT:
      return kGtU;
    case WASM_OPCODE_I32_SGE:
    case WASM_OPCODE_I64_SGE:
      return kGeS;
    case WASM_OPCODE_I32_UGE:
    case WASM_OPCODE_I64_UGE:
      return kGeU;
    case WASM_OPCODE_F32_LT:
    case WASM_OPCODE_F64_LT:
      return kLt;
    case WASM_OPCODE_F32_LE:
    case WASM_OPCODE_F64_LE:
      return kLe;
    case WASM_OPCODE_F32_GT:
    case WASM_OPCODE_F64_GT:
      return kGt;
    case WASM_OPCODE_F32_GE:
    case WASM_OPCODE_F64_GE:
      return kGe;
    default:
      assert(false && "Unexpected opcode in before_compare");
  }
}

void Parser::before_compare(WasmOpcode opcode) {
  auto* expr = new Expression(Expression::kCompare);
  expr->expr_type = Type::kI32;
  expr->compare_type = CompareType(opcode);
  expr->relop = CmpOperator(opcode);
  InsertAndPush(expr, 2);
}

static Type ConversionResultType(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_SCONVERT_F32:
    case WASM_OPCODE_I32_SCONVERT_F64:
    case WASM_OPCODE_I32_UCONVERT_F32:
    case WASM_OPCODE_I32_UCONVERT_F64:
    case WASM_OPCODE_I32_CONVERT_I64:
    case WASM_OPCODE_I32_REINTERPRET_F32:
      return Type::kI32;
    case WASM_OPCODE_I64_SCONVERT_F32:
    case WASM_OPCODE_I64_SCONVERT_F64:
    case WASM_OPCODE_I64_UCONVERT_F32:
    case WASM_OPCODE_I64_UCONVERT_F64:
    case WASM_OPCODE_I64_SCONVERT_I32:
    case WASM_OPCODE_I64_UCONVERT_I32:
    case WASM_OPCODE_I64_REINTERPRET_F64:
      return Type::kI64;
    case WASM_OPCODE_F32_SCONVERT_I32:
    case WASM_OPCODE_F32_UCONVERT_I32:
    case WASM_OPCODE_F32_SCONVERT_I64:
    case WASM_OPCODE_F32_UCONVERT_I64:
    case WASM_OPCODE_F32_CONVERT_F64:
    case WASM_OPCODE_F32_REINTERPRET_I32:
      return Type::kF32;
    case WASM_OPCODE_F64_SCONVERT_I32:
    case WASM_OPCODE_F64_UCONVERT_I32:
    case WASM_OPCODE_F64_SCONVERT_I64:
    case WASM_OPCODE_F64_UCONVERT_I64:
    case WASM_OPCODE_F64_CONVERT_F32:
    case WASM_OPCODE_F64_REINTERPRET_I64:
      return Type::kF64;
    default:
      assert(false && "Unexpected opcode in before_convert");
  }
}

static ConversionOperator ConvOperator(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_SCONVERT_F32:
    case WASM_OPCODE_I64_SCONVERT_F32:
      return kTruncSFloat32;
    case WASM_OPCODE_I32_SCONVERT_F64:
    case WASM_OPCODE_I64_SCONVERT_F64:
      return kTruncSFloat64;
    case WASM_OPCODE_I32_UCONVERT_F32:
    case WASM_OPCODE_I64_UCONVERT_F32:
      return kTruncUFloat32;
    case WASM_OPCODE_I32_UCONVERT_F64:
    case WASM_OPCODE_I64_UCONVERT_F64:
      return kTruncUFloat64;
    case WASM_OPCODE_I32_REINTERPRET_F32:
    case WASM_OPCODE_I64_REINTERPRET_F64:
      return kReinterpretFloat;
    case WASM_OPCODE_I32_CONVERT_I64:
      return kWrapInt64;
    case WASM_OPCODE_I64_SCONVERT_I32:
      return kExtendSInt32;
    case WASM_OPCODE_I64_UCONVERT_I32:
      return kExtendUInt32;
    case WASM_OPCODE_F32_SCONVERT_I32:
    case WASM_OPCODE_F64_SCONVERT_I32:
      return kConvertSInt32;
    case WASM_OPCODE_F32_UCONVERT_I32:
    case WASM_OPCODE_F64_UCONVERT_I32:
      return kConvertUInt32;
    case WASM_OPCODE_F32_SCONVERT_I64:
    case WASM_OPCODE_F64_SCONVERT_I64:
      return kConvertSInt64;
    case WASM_OPCODE_F32_UCONVERT_I64:
    case WASM_OPCODE_F64_UCONVERT_I64:
      return kConvertUInt64;
    case WASM_OPCODE_F32_CONVERT_F64:
      return kDemoteFloat64;
    case WASM_OPCODE_F32_REINTERPRET_I32:
    case WASM_OPCODE_F64_REINTERPRET_I64:
      return kReinterpretInt;
    case WASM_OPCODE_F64_CONVERT_F32:
      return kPromoteFloat32;
    default:
      assert(false && "Unexpected opcode in before_convert");
  }
}

static Type ConversionOperandType(Type result_type, ConversionOperator cvt) {
  switch (cvt) {
    case kExtendSInt32:
    case kExtendUInt32:
    case kConvertSInt32:
    case kConvertUInt32:
      return Type::kI32;
    case kConvertSInt64:
    case kConvertUInt64:
    case kWrapInt64:
      return Type::kI64;
    case kReinterpretInt:
      return result_type == Type::kF32 ? Type::kI32 : Type::kI64;
    case kTruncSFloat32:
    case kTruncUFloat32:
    case kPromoteFloat32:
      return Type::kF32;
    case kTruncSFloat64:
    case kTruncUFloat64:
    case kDemoteFloat64:
      return Type::kF64;
    case kReinterpretFloat:
      return result_type == Type::kI32 ? Type::kF32 : Type::kF64;
    default:
      assert(false && "Unexpected operator in before_convert");
  }
}

void Parser::before_convert(WasmOpcode opcode) {
  auto* expr = new Expression(Expression::kConvert);
  expr->expr_type = ConversionResultType(opcode);
  expr->cvt = ConvOperator(opcode);
  expr->operand_type = ConversionOperandType(expr->expr_type, expr->cvt);
  InsertAndPush(expr, 1);
}

void Parser::before_function() {
  current_func_ = functions_[current_callback_info_->function];
  current_type_ = current_func_->result_type;
  ResetInsertionPoint(&current_func_->body, kUnknownExpectedExprs);
}

void Parser::after_function(int num_exprs) {
  /* TODO: move this to a separate pass/prepass?*/
  // Desugar from a top-level list of exprs to an implicit block expr
  if (desugar_) {
    Function* func = functions_[current_callback_info_->function];
    if (func->body.size() > 1) {
      auto* expr = new Expression(Expression::kBlock);
      std::swap(expr->exprs, func->body);
      assert(insertion_points_.size() == 1);
      insertion_points_[0].point = &func->body;
      Insert(expr);
    } else if (func->body.empty()) {
      Insert(new Expression(Expression::kNop));
    }
  }
  PopInsertionPoint();
  current_func_ = nullptr;
}

void Parser::before_module() {
  assert(!module);
  WasmModule* m(current_callback_info_->module);
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
    module->imports.emplace_back(new Import(
        m->signatures.data[parser_import.signature_index].result_type,
        parser_import.module_name, parser_import.func_name));
    Import* imp = module->imports.back().get();
    for (size_t j = 0;
         j < m->signatures.data[parser_import.signature_index].args.size; ++j) {
      imp->locals.emplace_back(new Variable(
          m->signatures.data[parser_import.signature_index].args.data[j].type));
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

void Parser::after_module() {
  TypeChecker checker = {};
  checker.Visit(*module);
  module = nullptr;
}

void Parser::after_export(const char* export_name) {
  Function* func = functions_[current_callback_info_->function];
  assert(current_callback_info_->function);
  assert(export_name);
  module->exports.emplace_back(new Export(func, export_name, module));
  exports_by_name_.emplace(std::string(export_name),
                           module->exports.back().get());
}

void Parser::before_invoke(const char* invoke_name, int invoke_function_index) {
  assert(modules.size());
  Module* last_module = modules.back().get();
  auto* expr = new TestScriptExpr(last_module, TestScriptExpr::kInvoke,
                                  current_callback_info_->loc);
  if (current_assert_return_) {
    current_assert_return_->invoke.reset(expr);
  } else {
    test_script.emplace_back(expr);
  }
  assert(exports_by_name_.count(std::string(invoke_name)));
  expr->callee = exports_by_name_.find(std::string(invoke_name))->second;
  expr->type = expr->callee->function->result_type;
  assert(last_module->functions[invoke_function_index].get() ==
         expr->callee->function);
  PushInsertionPoint(&expr->exprs, expr->callee->function->args.size());
  SetCookie<TestScriptExpr*>(expr);
}

void Parser::after_invoke() {
  TypeChecker checker = {};
  checker.Visit(GetCookie<TestScriptExpr*>());
}

void Parser::before_assert_return() {
  assert(modules.size() && !module);
  Module* last_module = modules.back().get();
  test_script.emplace_back(new TestScriptExpr(
      last_module, TestScriptExpr::kAssertReturn, current_callback_info_->loc));
  current_assert_return_ = test_script.back().get();
  ResetInsertionPoint(&current_assert_return_->exprs, kUnknownExpectedExprs);
  SetCookie<TestScriptExpr*>(test_script.back().get());
}

void Parser::after_assert_return(WasmType ty) {
  auto* expr = GetCookie<TestScriptExpr*>();
  // The parser has already checked the types. We just need to propagate the
  // expectations down to the expectation expr tree.
  expr->type = ty;
  expr->invoke->type = ty;
  TypeChecker checker = {};
  if (expr->exprs.size()) {
    assert(ty != WASM_TYPE_VOID);
    expr->exprs.front()->expected_type = ty;
    checker.VisitExpression(expr->exprs.front().get());
  } else {
    assert(ty == WASM_TYPE_VOID);
  }
  checker.Visit(expr);
  PopInsertionPoint();
}

void Parser::before_assert_return_nan() {
  assert(modules.size() && !module);
  Module* last_module = modules.back().get();
  test_script.emplace_back(new TestScriptExpr(last_module,
                                              TestScriptExpr::kAssertReturnNaN,
                                              current_callback_info_->loc));
  current_assert_return_ = test_script.back().get();
  SetCookie<TestScriptExpr*>(test_script.back().get());
}

void Parser::after_assert_return_nan(WasmType ty) {
  auto* expr = GetCookie<TestScriptExpr*>();
  expr->type = ty;
  expr->invoke->type = ty;
  TypeChecker checker = {};
  checker.Visit(expr);
}

void Parser::before_assert_trap() {
  assert(modules.size() && !module);
  Module* last_module = modules.back().get();
  test_script.emplace_back(new TestScriptExpr(
      last_module, TestScriptExpr::kAssertTrap, current_callback_info_->loc));
  current_assert_return_ = test_script.back().get();
}

}  // namespace wasm
