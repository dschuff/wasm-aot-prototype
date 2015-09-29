#include "ast_dumper.h"

#include <cassert>
#include <cstdio>

static const char* TypeName(WasmType t) {
  switch (t) {
    case WASM_TYPE_VOID:
      return "void";
    case WASM_TYPE_I32:
      return "i32";
    case WASM_TYPE_I64:
      return "i64";
    case WASM_TYPE_F32:
      return "f32";
    case WASM_TYPE_F64:
      return "f64";
    default:
      return "(unknown type)";
  }
}

namespace wasm {

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
  if (c.result_type != WASM_TYPE_VOID)
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

static void dump_var_list(const UniquePtrVector<Variable>& lst,
                          const char* name) {
  for (auto& var : lst) {
    printf(" (%s", name);
    if (var->local_name.size())
      printf(" %s", var->local_name.c_str());
    printf(" %s)", TypeName(var->type));
  }
}

void AstDumper::VisitFunction(const Function& func) {
  printf("  (func ");
  if (func.local_name.size())
    printf("%s", func.local_name.c_str());

  dump_var_list(func.args, "param");
  dump_result(func);
  dump_var_list(func.locals, "local");
  for (auto& expr : func.body) {
    VisitExpression(*expr);
  }
  printf(")\n");
}

void AstDumper::VisitSegment(const Segment& seg) {
    printf("(segment %zu \"%s\")\n",
           seg.address, seg.as_string().c_str());
}


void AstDumper::VisitNop() {
  printf("(nop)");
}

void AstDumper::VisitBlock(const UniquePtrVector<Expression>& exprs) {
  printf("(block ");
  for (auto& e : exprs) {
    VisitExpression(*e);
  }
  printf(") ");
}

void AstDumper::VisitCall(bool is_import,
                          const Callable& callee,
                          int callee_index,
                          const UniquePtrVector<Expression>& args) {
  printf(is_import ? "(call_import " : "(call ");
  if (callee.local_name.size()) {
    printf("%s ", callee.local_name.c_str());
  } else {
    printf("%d ", callee_index);
  }
  for (auto& e : args) {
    VisitExpression(*e);
  }
  printf(") ");
}

void AstDumper::VisitReturn(const UniquePtrVector<Expression>& value) {
  printf("(return ");
  if (value.size())
    VisitExpression(*value.front());
  printf(") ");
}

void AstDumper::VisitGetLocal(const Variable& var) {
  printf("(get_local ");
  if (!var.local_name.empty()) {
    printf("%s)", var.local_name.c_str());
  } else {
    printf("%d)", var.index);
  }
}

void AstDumper::VisitSetLocal(const Variable& var, const Expression& value) {}

void AstDumper::VisitConst(const Literal& l) {
  switch (l.type) {
    case WASM_TYPE_I32:
      printf("(%s.const 0x%x)", TypeName(l.type), l.value.i32);
      break;
    case WASM_TYPE_I64:
      printf("(%s.const 0x%lx)", TypeName(l.type), l.value.i64);
      break;
    case WASM_TYPE_F32:
      printf("(%s.const %a)", TypeName(l.type), l.value.f32);
      break;
    case WASM_TYPE_F64:
      printf("(%s.const %a)", TypeName(l.type), l.value.f64);
      break;
    default:
      printf("unexpected type %d\n", l.type);
      assert(false);
  }
}

void AstDumper::VisitInvoke(const Export& callee,
                            const UniquePtrVector<Expression>& args) {
  printf("(invoke \"%s\" ", callee.name.c_str());
  for (auto& e : args) {
    VisitExpression(*e);
  }
  printf(")\n");
}

void AstDumper::VisitAssertEq(const TestScriptExpr& invoke_arg,
                              const Expression& expected) {
  printf("(assert_eq ");
  Visit(invoke_arg);
  VisitExpression(expected);
  printf(")\n");
}

} // namespace wasm
