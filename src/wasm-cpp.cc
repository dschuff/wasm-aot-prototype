#include "wasm-cpp.h"
#include <cstdio>
#include <cassert>

namespace wasm {

void Parser::after_nop() {
  insert(new Expression(WASM_OP_NOP));
}

WasmParserCookie Parser::before_block() {
  auto* expr = new Expression(WASM_OP_BLOCK);
  insert(expr);
  insertion_point_ = &expr->exprs;
  return 0;
}

void Parser::after_block(int num_exprs, WasmParserCookie cookie) {
  insertion_point_ = nullptr;
}

void Parser::after_const(WasmOpcode opcode, WasmType ty, WasmNumber value) {
  auto* expr = new Expression(WASM_OP_CONST);
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
  insert(expr);
}

void Parser::before_function(WasmModule* m, WasmFunction* f) {
  insertion_point_ = &functions_[f]->body;
}

void Parser::after_function(WasmModule* m, WasmFunction* f, int num_exprs) {
  /* TODO: move this to a separate pass/prepass?*/
  // Desugar from a top-level list of exprs to an implicit block expr
  if (desugar_) {
    Function* func = functions_[f];
    if (func->body.size() > 1) {
      auto* expr = new Expression(WASM_OP_BLOCK);
      std::swap(expr->exprs, func->body);
      insertion_point_ = &func->body;
      insert(expr);
    } else if (func->body.empty()) {
      insert(new Expression(WASM_OP_NOP));
    }
  }

  insertion_point_ = nullptr;
}

void Parser::before_module(WasmModule* m) {
  module.initial_memory_size = m->initial_memory_size;
  module.max_memory_size = m->max_memory_size;
  assert(module.max_memory_size >= module.initial_memory_size);

  module.functions.reserve(m->functions.size);
  for (size_t i = 0; i < m->functions.size; ++i) {
    WasmFunction* parser_func = &m->functions.data[i];
    module.functions.emplace_back();
    Function& func = module.functions.back();
    functions_[parser_func] = &func;

    func.index_in_module = i;
    func.result_type = parser_func->result_type;

    func.args.reserve(parser_func->num_args);
    for (int j = 0; j < parser_func->num_args; ++j) {
      func.args.emplace_back();
      func.args.back().type = parser_func->locals.data[j].type;
    }
    func.locals.reserve(parser_func->locals.size - parser_func->num_args);
    for (size_t j = parser_func->num_args; j < parser_func->locals.size; ++j) {
      func.locals.emplace_back();
      func.locals.back().type = parser_func->locals.data[j].type;
    }
    for (size_t j = 0; j < parser_func->local_bindings.size; ++j) {
      WasmBinding& binding = parser_func->local_bindings.data[j];
      if (binding.index < parser_func->num_args) {
        func.args[binding.index].local_name.assign(binding.name);
      } else {
        func.locals[binding.index - parser_func->num_args].local_name.assign(
            binding.name);
      }
    }
  }

  for (size_t i = 0; i < m->function_bindings.size; ++i) {
    WasmBinding& binding = m->function_bindings.data[i];
    module.functions[binding.index].local_name.assign(binding.name);
  }

  for (size_t i = 0; i < m->imports.size; ++i) {
    WasmImport& parser_import = m->imports.data[i];
    module.imports.emplace_back();
    Import& imp = module.imports.back();
    imp.module_name.assign(parser_import.module_name);
    imp.func_name.assign(parser_import.func_name);
    imp.result_type = parser_import.result_type;
    for (size_t j = 0; j < parser_import.args.size; ++j) {
      imp.args.emplace_back();
      imp.args.back().type = parser_import.args.data[j].type;
    }
  }
  for (size_t i = 0; i < m->import_bindings.size; ++i) {
    WasmBinding& binding = m->import_bindings.data[i];
    Import& imp = module.imports[binding.index];
    imp.local_name.assign(binding.name);
  }

  if (m->segments.size) {
    assert(module.initial_memory_size > 0);
    module.segments.reserve(m->segments.size);
  }
  for (size_t i = 0; i < m->segments.size; ++i) {
    WasmSegment& parser_seg = m->segments.data[i];
    module.segments.emplace_back();
    Segment& seg = module.segments.back();
    seg.size = parser_seg.size;
    seg.address = parser_seg.address;
    seg.initial_data.resize(seg.size);
    size_t copied = wasm_copy_string_contents(parser_seg.data,
                                              &seg.initial_data[0], seg.size);
    assert(copied == seg.size);
  }
}

void Parser::after_export(WasmModule* m, WasmExport* e) {
  Function* f = &module.functions[e->index];
  f->export_name.assign(e->name);
  module.exports.push_back(f);
}
}
