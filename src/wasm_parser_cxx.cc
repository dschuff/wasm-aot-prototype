#include "wasm_parser_cxx.h"
#include <cstdio>
#include <cassert>

namespace wasm {

void Parser::error(WasmSourceLocation loc, const char* msg) {
  fprintf(stderr, "%s:%d:%d: %s", loc.source->filename, loc.line, loc.col, msg);
}

void Parser::after_nop() {
  Insert(new Expression(WASM_OPCODE_NOP));
}

WasmParserCookie Parser::before_block() {
  auto* expr = new Expression(WASM_OPCODE_BLOCK);
  // TODO:This is ugly. Is block the only thing with an unknown number of exprs?
  InsertAndPush(expr, -1);
  return 0;
}

void Parser::after_block(int num_exprs, WasmParserCookie cookie) {
  PopInsertionPoint();
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
  InsertAndPush(expr, -1);
}

void Parser::after_return(WasmType ty) {
  PopInsertionPoint();
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

void Parser::before_function(WasmModule* m, WasmFunction* f) {
  ResetInsertionPoint(&functions_[f]->body, -1);
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
}

void Parser::before_module(WasmModule* m) {
  assert(!module);
  modules.emplace_back(new Module());
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

    func->args.reserve(parser_func->num_args);
    for (int j = 0; j < parser_func->num_args; ++j) {
      func->args.emplace_back(new Variable(parser_func->locals.data[j].type));
    }
    func->locals.reserve(parser_func->locals.size - parser_func->num_args);
    for (size_t j = parser_func->num_args; j < parser_func->locals.size; ++j) {
      func->locals.emplace_back(new Variable(parser_func->locals.data[j].type));
    }
    for (size_t j = 0; j < parser_func->local_bindings.size; ++j) {
      WasmBinding& binding = parser_func->local_bindings.data[j];
      if (binding.index < parser_func->num_args) {
        func->args[binding.index]->local_name.assign(binding.name);
      } else {
        func->locals[binding.index - parser_func->num_args]->local_name.assign(
            binding.name);
      }
    }
  }

  for (size_t i = 0; i < m->function_bindings.size; ++i) {
    WasmBinding& binding = m->function_bindings.data[i];
    module->functions[binding.index]->local_name.assign(binding.name);
  }

  for (size_t i = 0; i < m->imports.size; ++i) {
    WasmImport& parser_import = m->imports.data[i];
    module->imports.emplace_back(new Import(parser_import.result_type,
                                            parser_import.module_name,
                                            parser_import.func_name));
    Import* imp = module->imports.back().get();
    for (size_t j = 0; j < parser_import.args.size; ++j) {
      imp->args.emplace_back(new Variable(parser_import.args.data[j].type));
    }
  }
  for (size_t i = 0; i < m->import_bindings.size; ++i) {
    WasmBinding& binding = m->import_bindings.data[i];
    Import* imp = module->imports[binding.index].get();
    imp->local_name.assign(binding.name);
  }

  if (m->segments.size) {
    assert(module->initial_memory_size > 0);
    module->segments.reserve(m->segments.size);
  }
  for (size_t i = 0; i < m->segments.size; ++i) {
    WasmSegment& parser_seg = m->segments.data[i];
    module->segments.emplace_back(
        new Segment(parser_seg.size, parser_seg.address));
    Segment* seg = module->segments.back().get();
    seg->initial_data.resize(seg->size);
    wasm_copy_segment_data(parser_seg.data, &seg->initial_data[0], seg->size);
  }
}

void Parser::after_module(WasmModule* m) {
  module = nullptr;
}

void Parser::after_export(WasmModule* m, WasmFunction* f) {
  Function* func = functions_[f];
  assert(f->exported_name);
  module->exports.emplace_back(new Export(func, f->exported_name, module));
}

void Parser::before_invoke(const char* invoke_name, int invoke_function_index) {
  assert(modules.size());
  Module* last_module = modules.back().get();
  auto* expr = new TestScriptExpr(last_module, TestScriptExpr::kInvoke);
  if (current_assert_eq_) {
    current_assert_eq_->invoke.reset(expr);
  } else {
    test_script.emplace_back(expr);
  }
  expr->callee = last_module->exports[invoke_function_index].get();
  PushInsertionPoint(&expr->exprs, expr->callee->function->args.size());
}

WasmParserCookie Parser::before_assert_eq() {
  assert(modules.size() && !module);
  Module* last_module = modules.back().get();
  test_script.emplace_back(
      new TestScriptExpr(last_module, TestScriptExpr::kAssertEq));
  current_assert_eq_ = test_script.back().get();
  ResetInsertionPoint(&current_assert_eq_->exprs, 1);
  return 0;
}
}  // namespace wasm
