#include "ast_dumper.h"

#include <cassert>
#include <cstdio>

static const char* TypeName(wasm::Type t) {
  switch (t) {
    case wasm::Type::kVoid:
      return "void";
    case wasm::Type::kI32:
      return "i32";
    case wasm::Type::kI64:
      return "i64";
    case wasm::Type::kF32:
      return "f32";
    case wasm::Type::kF64:
      return "f64";
    case wasm::Type::kAny:
      return "(any)";
    default:
      return "(unknown)";
  }
}

namespace wasm {
// Freestanding utility function to dump an expression for debugging.
void DumpExpr(Expression* expr, bool dump_types) {
  AstDumper dumper(dump_types);
  dumper.VisitExpression(expr);
}

void AstDumper::PrintType(const Expression& expr) {
  if (dump_types_)
    printf("[%s->%s]", TypeName(expr.expected_type), TypeName(expr.expr_type));
}

void AstDumper::VisitModule(const Module& mod) {
  printf("(module\n");
  for (auto& func : mod.functions)
    VisitFunction(*func);

  if (mod.initial_memory_size) {
    printf("(memory %u", mod.initial_memory_size);
    if (mod.max_memory_size)
      printf(" %u ", mod.max_memory_size);
    for (auto& seg : mod.segments)
      VisitSegment(*seg);
    printf(")\n");
  }

  for (auto& imp : mod.imports)
    VisitImport(*imp);
  for (auto& ex : mod.exports)
    VisitExport(*ex);

  printf(")\n");
}

// Functions can be declared with a single parameter list like
// (func (param i32 i64)) or with a split parameter list like
// (func (param i32) (param i32)
// If any of the params have name bindings e.g. (param $n i32) then the
// style must be used. However imports are required to use the single
// style. If this is fixed, arg dumping can be shared more.
static void dump_result(const Callable& c) {
  if (c.result_type != Type::kVoid)
    printf(" (result %s)", TypeName(c.result_type));
}

void AstDumper::VisitImport(const Import& import) {
  printf("(import %s \"%s\" \"%s\"",
         import.local_name.c_str(), import.module_name.c_str(),
         import.func_name.c_str());

  if (import.args.size()) {
    printf(" (param");
    for (auto& arg : import.args)
      printf(" %s", TypeName(arg->type));
    printf(")");
  }
  dump_result(import);
  printf(")\n");
}

void AstDumper::VisitExport(const Export& exp) {
  printf("(export \"%s\" ", exp.name.c_str());
  if (exp.function->local_name.empty()) {
    printf("%u)", exp.function->index_in_module);
  } else {
    printf("%s)", exp.function->local_name.c_str());
  }
}

template <typename T>
static void dump_var_list(T&& begin, T&& end, const char* name) {
  for (auto& var = begin; var != end; ++var) {
    printf(" (%s", name);
    if ((*var)->local_name.size())
      printf(" %s", (*var)->local_name.c_str());
    printf(" %s)", TypeName((*var)->type));
  }
}

void AstDumper::VisitFunction(const Function& func) {
  printf("  (func ");
  if (func.local_name.size())
    printf("%s", func.local_name.c_str());

  dump_var_list(func.args.begin(), func.args.end(), "param");
  dump_result(func);
  dump_var_list(func.locals.begin() + func.args.size(), func.locals.end(),
                "local");
  for (auto& expr : func.body) {
    VisitExpression(expr.get());
  }
  printf(")\n");
}

void AstDumper::VisitSegment(const Segment& seg) {
    printf("(segment %zu \"%s\")\n",
           seg.address, seg.as_string().c_str());
}

void AstDumper::VisitNop(Expression* expr) {
  printf("(nop)");
}

void AstDumper::VisitBlock(Expression* expr,
                           UniquePtrVector<Expression>* exprs) {
  printf("(block ");
  for (auto& e : *exprs) {
    VisitExpression(e.get());
  }
  printf(") ");
}

void AstDumper::VisitIf(Expression* expr,
                        Expression* condition,
                        Expression* then,
                        Expression* els) {
  printf("(if ");
  VisitExpression(condition);
  VisitExpression(then);
  if (els)
    VisitExpression(els);
  printf(")\n");
}

void AstDumper::VisitCall(Expression* expr,
                          bool is_import,
                          Callable* callee,
                          int callee_index,
                          UniquePtrVector<Expression>* args) {
  printf(is_import ? "(call_import " : "(call ");
  if (callee->local_name.size()) {
    printf("%s ", callee->local_name.c_str());
  } else {
    printf("%d ", callee_index);
  }
  for (auto& e : *args) {
    VisitExpression(e.get());
  }
  printf(") ");
}

void AstDumper::VisitReturn(Expression* expr,
                            UniquePtrVector<Expression>* value) {
  printf("(return ");
  if (value->size())
    VisitExpression(value->front().get());
  printf(") ");
}

void AstDumper::VisitGetLocal(Expression* expr, Variable* var) {
  printf("(get_local ");
  if (!var->local_name.empty()) {
    printf("%s)", var->local_name.c_str());
  } else {
    printf("%d)", var->index);
  }
}

void AstDumper::VisitSetLocal(Expression* expr,
                              Variable* var,
                              Expression* value) {
  printf("(set_local ");
  if (!var->local_name.empty()) {
    printf("%s ", var->local_name.c_str());
  } else {
    printf("%d ", var->index);
  }
  VisitExpression(value);
  printf(")");
}

void AstDumper::VisitConst(Expression* expr, Literal* l) {
  switch (l->type) {
    case Type::kI32:
      printf("(%s.const 0x%x)", TypeName(l->type), l->value.i32);
      break;
    case Type::kI64:
      printf("(%s.const 0x%lx)", TypeName(l->type), l->value.i64);
      break;
    case Type::kF32:
      printf("(%s.const %a)", TypeName(l->type), l->value.f32);
      break;
    case Type::kF64:
      printf("(%s.const %a)", TypeName(l->type), l->value.f64);
      break;
    default:
      printf("unexpected type %d\n", static_cast<int>(l->type));
      assert(false);
  }
}

static const char* BinopName(BinaryOperator binop) {
  switch (binop) {
    case kAdd:
      return "add";
    case kSub:
      return "sub";
    case kMul:
      return "mul";
    case kDivS:
      return "div_s";
    case kDivU:
      return "div_u";
    case kRemS:
      return "rem_s";
    case kRemU:
      return "rem_u";
    case kAnd:
      return "and";
    case kOr:
      return "or";
    case kXor:
      return "xor";
    case kShl:
      return "shl";
    case kShrU:
      return "shr_u";
    case kShrS:
      return "shr_s";
    case kDiv:
      return "div";
    case kCopySign:
      return "copysign";
    case kMin:
      return "min";
    case kMax:
      return "max";
    default:
      assert(false);
  }
}

void AstDumper::VisitBinop(Expression* expr,
                           BinaryOperator binop,
                           Expression* lhs,
                           Expression* rhs) {
  printf("(%s.%s ", TypeName(expr->expr_type), BinopName(binop));
  VisitExpression(lhs);
  VisitExpression(rhs);
  printf(")\n");
}

static const char* CompareOpName(CompareOperator relop) {
  switch (relop) {
    case kEq:
      return "eq";
    case kNE:
      return "ne";
    case kLtS:
      return "lt_s";
    case kLtU:
      return "lt_u";
    case kLeS:
      return "le_s";
    case kLeU:
      return "le_u";
    case kGtS:
      return "gt_s";
    case kGtU:
      return "gt_u";
    case kGeS:
      return "ge_s";
    case kGeU:
      return "ge_u";
    case kLt:
      return "lt";
    case kLe:
      return "le";
    case kGt:
      return "gt";
    case kGe:
      return "ge";
    default:
      assert(false);  // The switch is covered but gcc still warns :(
  }
}

void AstDumper::VisitCompare(Expression* expr,
                             Type compare_type,
                             CompareOperator relop,
                             Expression* lhs,
                             Expression* rhs) {
  printf("(%s.%s ", TypeName(compare_type), CompareOpName(relop));
  VisitExpression(lhs);
  VisitExpression(rhs);
  printf(")\n");
}

void AstDumper::VisitInvoke(TestScriptExpr* expr,
                            Export* callee,
                            UniquePtrVector<Expression>* args) {
  printf("(invoke \"%s\" ", callee->name.c_str());
  for (auto& e : *args) {
    VisitExpression(e.get());
  }
  printf(")\n");
}

void AstDumper::VisitAssertReturn(TestScriptExpr* expr,
                                  TestScriptExpr* invoke_arg,
                                  Expression* expected) {
  printf("(assert_return ");
  Visit(invoke_arg);
  VisitExpression(expected);
  printf(")\n");
}

} // namespace wasm
