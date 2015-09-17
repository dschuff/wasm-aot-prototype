#include "wasm-ast.h"

#include <memory>
#include <cassert>

namespace wasm {

static const char* TypeName(WasmType t) {
  switch(t) {
    case WASM_TYPE_VOID: return "void";
    case WASM_TYPE_I32: return "i32";
    case WASM_TYPE_I64: return "i64";
    case WASM_TYPE_F32: return "f32";
    case WASM_TYPE_F64: return "f64";
    default: return "(unknown type)";
  }
}

void Expression::dump() {
  printf("(");
  switch(opcode) {
    case WASM_OP_NOP:
      printf("nop");
      break;
    case WASM_OP_BLOCK:
      printf("block ");
      for (auto& expr : exprs) {
        expr->dump();
      }
      break;
    default:
      assert(false);
  }
  printf(")");
}

void Callable::dump_result() {
  if (result_type != WASM_TYPE_VOID)
    printf(" (result %s)", TypeName(result_type));
}

void Callable::dump() {
  // For now, try to avoid having virtual tables in the IR classes (even though
  // it's maybe inevitable). Both Function and Import have dump() so disallow
  // calling dump() on the base class.
  assert(false);
}

void Function::dump_var_list(const std::vector<Variable>& lst,
                             const char* name) {
  for (auto &var : lst) {
    printf(" (%s", name);
    if (var.local_name.size())
      printf(" %s", var.local_name.c_str());
    printf(" %s)", TypeName(var.type));
  }
}

void Function::dump() {
  printf("  (func ");
  if (local_name.size())
    printf("%s", local_name.c_str());

  dump_var_list(args, "param");
  dump_result();
  dump_var_list(locals, "local");
  for (auto &expr : body) {
    expr->dump();
  }
  printf(")\n");
}

void Import::dump() {
  printf("(import %s \"%s\" \"%s\"",
         local_name.c_str(),
         module_name.c_str(),
         func_name.c_str());

  if (args.size()) {
    printf(" (param");
    for (auto &arg : args)
      printf(" %s", TypeName(arg.type));
    printf(")");
  }
  dump_result();
  printf(")\n");
}

void Module::dump() {
  printf("(module\n");
  for (auto& func : functions)
    func.dump();

  if (initial_memory_size) {
    printf("(memory %u", initial_memory_size);
    if (max_memory_size)
      printf(" %u ", max_memory_size);
    for (auto& seg : segments)
      seg.dump();
    printf(")\n");
  }

  for (auto& imp : imports)
    imp.dump();
  for (auto* ex : exports)
    printf("(export \"%s\" %u)", ex->export_name.c_str(), ex->index_in_module);

  printf(")\n");
}

} // namespace wasm
