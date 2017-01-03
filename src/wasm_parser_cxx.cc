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

#include "wasm_parser_cxx.h"
#include "ast_dumper.h"
#include "ast_visitor.h"
#include "support.h"
#include <cstdio>
#include <cassert>
#include <cstring>

namespace wasm {

// TODO: The opcodes are now a single type/space in WABT. Merge all these
// FooOperator functions?
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
    case WASM_OPCODE_I32_DIV_S:
    case WASM_OPCODE_I64_DIV_S:
      return kDivS;
    case WASM_OPCODE_I32_DIV_U:
    case WASM_OPCODE_I64_DIV_U:
      return kDivU;
    case WASM_OPCODE_I32_REM_S:
    case WASM_OPCODE_I64_REM_S:
      return kRemS;
    case WASM_OPCODE_I32_REM_U:
    case WASM_OPCODE_I64_REM_U:
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
    case WASM_OPCODE_I32_SHR_U:
    case WASM_OPCODE_I64_SHR_U:
      return kShrU;
    case WASM_OPCODE_I32_SHR_S:
    case WASM_OPCODE_I64_SHR_S:
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
    case WASM_OPCODE_I32_LT_S:
    case WASM_OPCODE_I64_LT_S:
      return kLtS;
    case WASM_OPCODE_I32_LE_S:
    case WASM_OPCODE_I64_LE_S:
      return kLeS;
    case WASM_OPCODE_I32_LT_U:
    case WASM_OPCODE_I64_LT_U:
      return kLtU;
    case WASM_OPCODE_I32_LE_U:
    case WASM_OPCODE_I64_LE_U:
      return kLeU;
    case WASM_OPCODE_I32_GT_S:
    case WASM_OPCODE_I64_GT_S:
      return kGtS;
    case WASM_OPCODE_I32_GT_U:
    case WASM_OPCODE_I64_GT_U:
      return kGtU;
    case WASM_OPCODE_I32_GE_S:
    case WASM_OPCODE_I64_GE_S:
      return kGeS;
    case WASM_OPCODE_I32_GE_U:
    case WASM_OPCODE_I64_GE_U:
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
      assert(false && "Unexpected opcode in CmpOperator");
  }
}

static ConversionOperator ConvOperator(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_TRUNC_S_F32:
    case WASM_OPCODE_I64_TRUNC_S_F32:
      return kTruncSFloat32;
    case WASM_OPCODE_I32_TRUNC_S_F64:
    case WASM_OPCODE_I64_TRUNC_S_F64:
      return kTruncSFloat64;
    case WASM_OPCODE_I32_TRUNC_U_F32:
    case WASM_OPCODE_I64_TRUNC_U_F32:
      return kTruncUFloat32;
    case WASM_OPCODE_I32_TRUNC_U_F64:
    case WASM_OPCODE_I64_TRUNC_U_F64:
      return kTruncUFloat64;
    case WASM_OPCODE_I32_WRAP_I64:
      return kWrapInt64;
    case WASM_OPCODE_I64_EXTEND_S_I32:
      return kExtendSInt32;
    case WASM_OPCODE_I64_EXTEND_U_I32:
      return kExtendUInt32;
    case WASM_OPCODE_F32_CONVERT_S_I32:
    case WASM_OPCODE_F64_CONVERT_S_I32:
      return kConvertSInt32;
    case WASM_OPCODE_F32_CONVERT_U_I32:
    case WASM_OPCODE_F64_CONVERT_U_I32:
      return kConvertUInt32;
    case WASM_OPCODE_F32_CONVERT_S_I64:
    case WASM_OPCODE_F64_CONVERT_S_I64:
      return kConvertSInt64;
    case WASM_OPCODE_F32_CONVERT_U_I64:
    case WASM_OPCODE_F64_CONVERT_U_I64:
      return kConvertUInt64;
    case WASM_OPCODE_F32_DEMOTE_F64:
      return kDemoteFloat64;
    case WASM_OPCODE_F64_PROMOTE_F32:
      return kPromoteFloat32;
    case WASM_OPCODE_F32_REINTERPRET_I32:
    case WASM_OPCODE_F64_REINTERPRET_I64:
      return kReinterpretInt;
    case WASM_OPCODE_I32_REINTERPRET_F32:
    case WASM_OPCODE_I64_REINTERPRET_F64:
      return kReinterpretFloat;
    default:
      assert(false && "Unexpected opcode in ConvOperator");
  }
}

static MemType MemOperandType(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_LOAD8_S:
    case WASM_OPCODE_I32_LOAD8_U:
    case WASM_OPCODE_I32_STORE8:
    case WASM_OPCODE_I64_LOAD8_S:
    case WASM_OPCODE_I64_LOAD8_U:
    case WASM_OPCODE_I64_STORE8:
      return MemType::kI8;
    case WASM_OPCODE_I32_LOAD16_S:
    case WASM_OPCODE_I32_LOAD16_U:
    case WASM_OPCODE_I32_STORE16:
    case WASM_OPCODE_I64_LOAD16_S:
    case WASM_OPCODE_I64_LOAD16_U:
    case WASM_OPCODE_I64_STORE16:
      return MemType::kI16;
    case WASM_OPCODE_I32_LOAD:
    case WASM_OPCODE_I32_STORE:
    case WASM_OPCODE_I64_LOAD32_S:
    case WASM_OPCODE_I64_LOAD32_U:
    case WASM_OPCODE_I64_STORE32:
      return MemType::kI32;
    case WASM_OPCODE_I64_LOAD:
    case WASM_OPCODE_I64_STORE:
      return MemType::kI64;
    case WASM_OPCODE_F32_LOAD:
    case WASM_OPCODE_F32_STORE:
      return MemType::kF32;
    case WASM_OPCODE_F64_LOAD:
    case WASM_OPCODE_F64_STORE:
      return MemType::kF64;
    default:
      assert(false && "Unexpected opcode in MemOperandType");
  }
}

static bool IsMemOpSigned(WasmOpcode opcode) {
  switch (opcode) {
    case WASM_OPCODE_I32_LOAD8_S:
    case WASM_OPCODE_I32_LOAD16_S:
    case WASM_OPCODE_I64_LOAD8_S:
    case WASM_OPCODE_I64_LOAD16_S:
    case WASM_OPCODE_I64_LOAD32_S:
      return true;
    default:
      // Signedness is only meaningful for extending loads.
      return false;
  }
}

void Parser::ConvertExprArg(WasmExpr* in_expr,
                            Expression* out_expr,
                            Type expected_type) {
  out_expr->exprs.emplace_back(ConvertExpression(in_expr, expected_type));
}

static ConstantExpression* ConvertConstant(const WasmConst& in_const,
                                           Type expected_type) {
  auto* out_expr = new ConstantExpression(in_const.type, expected_type);
  switch (out_expr->expr_type) {
    case WASM_TYPE_I32:
      out_expr->literal.value.i32 = in_const.u32;
      return out_expr;
    case WASM_TYPE_I64:
      out_expr->literal.value.i64 = in_const.u64;
      return out_expr;
    case WASM_TYPE_F32:
      out_expr->literal.value.f32 = bit_cast<float>(in_const.f32_bits);
      return out_expr;
    case WASM_TYPE_F64:
      out_expr->literal.value.f64 = bit_cast<double>(in_const.f64_bits);
      return out_expr;
    default:
      assert(false);
  }
}

void Parser::ConvertBlockArgs(const WasmExpr* first,
                              UniquePtrVector<Expression>* out_vec,
                              Type block_sig) {
  const WasmExpr* cur = first;
  while(cur) {
    Type expected(cur->next ? (Type)Type::kVoid : block_sig);
    auto* e = ConvertExpression(cur, expected);
    if (e)
      out_vec->emplace_back(e);
    cur = cur->next;
  }
}

Expression* Parser::ConvertExpression(const WasmExpr* in_expr, Type expected_type) {
  switch (in_expr->type) {
    case WASM_EXPR_TYPE_CONST: {
      Push(ConvertConstant(in_expr->const_, expected_type));
      return nullptr;
    }
    case WASM_EXPR_TYPE_NOP:
      return new Expression(Expression::kNop, Type::kVoid, expected_type);
    case WASM_EXPR_TYPE_BLOCK: {
      WasmType ty = WASM_TYPE_VOID;
      if (in_expr->block.sig.size)
	ty = in_expr->block.sig.data[0];
      auto* out_expr =
          new Expression(Expression::kBlock, ty, expected_type);
      ConvertBlockArgs(in_expr->block.first, &out_expr->exprs, expected_type);
      return out_expr;
    }
    case WASM_EXPR_TYPE_RETURN: {
      auto* out_expr =
          new Expression(Expression::kReturn, Type::kUnknown, expected_type);
      if (out_func_->result_type != Type::kVoid) {
	out_expr->exprs.emplace_back(Pop());
        out_expr->expr_type = out_func_->result_type;
      }
      return out_expr;
    }
      /*case WASM_EXPR_TYPE_IF: {
      auto* out_expr =
          new Expression(Expression::kIf, Type::kUnknown, expected_type);
      ConvertExprArg(in_expr->if_.cond, out_expr, Type::kI32);
      ConvertExprArg(in_expr->if_.true_, out_expr, expected_type);
      out_expr->expr_type = out_expr->exprs.back()->expr_type;
      return out_expr;
    }
    case WASM_EXPR_TYPE_IF_ELSE: {
      auto* out_expr =
          new Expression(Expression::kIfElse, Type::kUnknown, expected_type);
      ConvertExprArg(in_expr->if_else.cond, out_expr, Type::kI32);
      ConvertExprArg(in_expr->if_else.true_, out_expr, expected_type);
      ConvertExprArg(in_expr->if_else.false_, out_expr, expected_type);
      out_expr->expr_type = out_expr->exprs.back()->expr_type;
n      return out_expr;
      }*/
    case WASM_EXPR_TYPE_CALL: {
      int index = wasm_get_func_index_by_var(in_module_, &in_expr->call.var);
      auto* callee = out_module_->functions[index].get();
      bool is_import = index < in_module_->num_func_imports;
      auto* out_expr = new CallExpression(index, is_import, callee, expected_type);
      for (size_t i = 0; i < callee->args.size(); ++i) {
	out_expr->exprs.emplace_back(Pop());
      }
      if (callee->result_type != Type::kVoid) {
	Push(out_expr);
	return nullptr;
      }
      return out_expr;
    }
    case WASM_EXPR_TYPE_DROP: {
      assert(expr_stack_.size() <= 1);
      if (expr_stack_.size())
	return Pop();
      return nullptr;
    }
    case WASM_EXPR_TYPE_GET_LOCAL: {
      int local_index =
          wasm_get_local_index_by_var(in_func_, &in_expr->get_local.var);
      Push(LocalExpression::GetGetLocal(out_func_->locals[local_index].get(),
					expected_type));
      return nullptr;
    }
    case WASM_EXPR_TYPE_SET_LOCAL: {
      int local_index =
          wasm_get_local_index_by_var(in_func_, &in_expr->set_local.var);
      auto* out_expr = LocalExpression::GetSetLocal(
          out_func_->locals[local_index].get(), expected_type);
      out_expr->exprs.emplace_back(Pop());
      return out_expr;
    }
    case WASM_EXPR_TYPE_LOAD: {
      auto* out_expr =
          new MemoryExpression(kLoad,
                               wasm_get_opcode_result_type(in_expr->load.opcode),
                               expected_type,
                               MemOperandType(in_expr->load.opcode),
                               in_expr->load.align,
                               in_expr->load.offset,
                               IsMemOpSigned(in_expr->load.opcode));
      out_expr->exprs.emplace_back(Pop());
      Push(out_expr);
      return nullptr;
    }
    case WASM_EXPR_TYPE_STORE: {
      auto* out_expr =
          new MemoryExpression(kStore,
                               wasm_get_opcode_result_type(in_expr->load.opcode),
                               expected_type,
                               MemOperandType(in_expr->store.opcode),
                               in_expr->store.align,
                               in_expr->store.offset,
                               false);
      out_expr->exprs.emplace_back(Pop());
      assert(out_expr->exprs.back()->expr_type == Type::kI32);
      out_expr->exprs.emplace_back(Pop());
      return out_expr;
    }
    case WASM_EXPR_TYPE_UNARY: {
      auto* out_expr =
          new UnaryExpression(UnopOperator(in_expr->unary.opcode),
                              wasm_get_opcode_result_type(in_expr->unary.opcode),
                              expected_type);
      out_expr->exprs.emplace_back(Pop());
      Push(out_expr);
      return nullptr;
    }
    case WASM_EXPR_TYPE_BINARY: {
      auto* out_expr =
          new BinaryExpression(BinopOperator(in_expr->binary.opcode),
                               wasm_get_opcode_result_type(in_expr->binary.opcode),
                               expected_type);
      out_expr->exprs.emplace_back(Pop());
      out_expr->exprs.emplace_back(Pop());
      Push(out_expr);
      return nullptr;
    }
    case WASM_EXPR_TYPE_COMPARE: {
      auto* out_expr =
          new CompareExpression(wasm_get_opcode_result_type(in_expr->compare.opcode),
                                CmpOperator(in_expr->compare.opcode),
                                expected_type);
      out_expr->exprs.emplace_back(Pop());
      out_expr->exprs.emplace_back(Pop());
      Push(out_expr);
      return nullptr;
    }
    case WASM_EXPR_TYPE_CONVERT: {
      auto* out_expr =
          new ConversionExpression(ConvOperator(in_expr->convert.opcode),
                                   wasm_get_opcode_param_type_1(in_expr->convert.opcode),
                                   wasm_get_opcode_result_type(in_expr->convert.opcode),
                                   expected_type);
      out_expr->exprs.emplace_back(Pop());
      Push(out_expr);
      return nullptr;
    }
    default:
      assert(false && "Unhandled expression type");
      return nullptr;
  }
}

Module* Parser::ConvertModule(WasmModule* in_mod) {
  in_module_ = in_mod;
  std::unique_ptr<Module> out_mod(new Module());
  out_module_ = out_mod.get();
  exports_map_.clear();
  // First set up module-level constructs: Segments, functions, imports, exports
  // Memory segment declarations and initializers
  /*
  if (in_mod->memories.size) {
    assert(in_mod->memories.size == 1);
    out_mod->initial_memory_size = in_mod->memories[0].page_limits.initial * wasm::Module::kPageSize;
    bool has_max = in_mod->memories[0].page_limits.has_max;
    out_mod->max_memory_size = has_max ? in_mod->memories[0].page_limits.max * wasm::Module::kPageSize : 0; // XXX: fix for !has_max
    if (in_mod->data_segments.size) {
      for (size_t i = 0; i < in_mod->data_segments.size; ++i) {
        const WasmSegment* in_seg = &in_mod->memory->segments.data[i];
        out_mod->segments.emplace_back(new Segment(in_seg->size, in_seg->addr));
        Segment* out_seg = out_mod->segments.back().get();
        out_seg->initial_data.resize(out_seg->size);
        memcpy(out_seg->initial_data.data(), in_seg->data, in_seg->size);
      }
    }
  }*/
  // Functions, with locals/params
  std::unordered_map<std::string, Function*> functions_by_name;
  out_mod->functions.reserve(in_mod->funcs.size);
  for (size_t i = 0; i < in_mod->funcs.size; ++i) {
    const WasmFunc* in_func = in_mod->funcs.data[i];
    WasmType result_ty = wasm_get_num_results(in_func) ? wasm_get_result_type(in_func, 0) : WASM_TYPE_VOID;
    out_mod->functions.emplace_back(
        new Function(result_ty, in_func->name, i));
    Function* out_func = out_mod->functions.back().get();
    if (in_func->name.length) {
      auto result = functions_by_name.emplace(out_func->local_name, out_func);
      assert(result.second);
    }

    size_t param_count = wasm_get_num_params(in_func);
    size_t local_count = wasm_get_num_locals(in_func);
    out_func->locals.reserve(local_count);
    out_func->args.reserve(param_count);
    for (size_t j = 0; j < param_count + local_count; ++j) {
      WasmType t;
      if (j < param_count) {
	t = wasm_get_param_type(in_func, j);
      } else {
	t = wasm_get_local_type(in_func, j - param_count);
      }
      out_func->locals.emplace_back(new Variable(t));
      Variable* out_var = out_func->locals.back().get();
      out_var->index = j;
      if (j < param_count)
        out_func->args.push_back(out_var);
    }
    // TODO: this is essentially duplicated from binary-writer.c
    // TODO: location info
    if (param_count + local_count) {
      WasmStringSliceVector index_to_name;
      WASM_ZERO_MEMORY(index_to_name);
      wasm_make_type_binding_reverse_mapping(
            allocator_, &in_func->decl.sig.param_types, &in_func->param_bindings,
            &index_to_name);
      for (size_t j = 0; j < param_count; ++j) {
	WasmStringSlice name = index_to_name.data[j];
	out_func->locals[j]->local_name.assign(name.start, name.length);
      }

      wasm_make_type_binding_reverse_mapping(
            allocator_, &in_func->local_types, &in_func->local_bindings,
            &index_to_name);
      for (size_t j = 0; j < wasm_get_num_locals(in_func); ++j) {
	WasmStringSlice name = index_to_name.data[j];
	out_func->locals[param_count + j]->local_name.assign(name.start, name.length);
      }
    }
  }
  // Imports, with signatures
  for (size_t i = 0; i < in_mod->imports.size; ++i) {
    const WasmImport* in_import = in_mod->imports.data[i];
    if (in_import->kind != WASM_EXTERNAL_KIND_FUNC) continue;
    const WasmFunc* func = &in_import->func;
    out_mod->imports.emplace_back(new Import(wasm_get_result_type(func, 0),
                                  in_import->field_name, // XXX: fix
                                  in_import->module_name,
                                  in_import->field_name));
    Import* imp = out_mod->imports.back().get();
    for (size_t j = 0; j < wasm_get_num_params(func); ++j) {
      imp->locals.emplace_back(
          new Variable(wasm_get_param_type(func, j)));
      imp->args.push_back(imp->locals.back().get());
    }
  }
  // Exports
  for (size_t i = 0; i < in_mod->exports.size; ++i) {
    const WasmExport* in_export = in_mod->exports.data[i];
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
    exports_map_.emplace(in_export, out_mod->exports.back().get());
  }

  // Convert the functions
  for (size_t i = 0; i < in_mod->funcs.size; ++i) {
    in_func_ = in_mod->funcs.data[i];
    out_func_ = out_mod->functions[i].get();
    ConvertBlockArgs(in_func_->first_expr, &out_func_->body, out_func_->result_type);
    in_func_ = nullptr;
    out_func_ = nullptr;
  }
  return out_mod.release();
}

TestScriptExpr* Parser::ConvertInvoke(const WasmAction& invoke) {
  assert(invoke.type == WASM_ACTION_TYPE_INVOKE);
  auto* expr =
      new TestScriptExpr(out_module_, TestScriptExpr::kInvoke, invoke.loc);
  WasmExport* ex = wasm_get_export_by_name(in_module_, &invoke.invoke.name);
  int invoke_index = wasm_get_func_index_by_var(in_module_, &ex->var);
  Export* callee = exports_map_[ex];
  expr->callee = callee;
  expr->type = callee->function->result_type;
  assert(out_module_->functions[invoke_index].get() == callee->function);
  for (size_t i = 0; i < invoke.invoke.args.size; ++i) {
    auto* arg_expr =
        ConvertConstant(invoke.invoke.args.data[i], callee->function->args[i]->type);
    expr->exprs.emplace_back(arg_expr);
  }
  return expr;
}

TestScriptExpr* Parser::ConvertTestScriptExpr(WasmCommand* command) {
  switch (command->type) {
    case WASM_COMMAND_TYPE_ACTION: {
      return ConvertInvoke(command->action);
    }
    case WASM_COMMAND_TYPE_ASSERT_RETURN: {
      auto* expr = new TestScriptExpr(out_module_,
                                      TestScriptExpr::kAssertReturn,
                                      command->assert_return.action.loc);
      expr->invoke.reset(ConvertInvoke(command->assert_return.action));
      if (command->assert_return.expected.size) {
        expr->exprs.emplace_back(
            ConvertConstant(command->assert_return.expected.data[0],
                            command->assert_return.expected.data[0].type));
        expr->type = expr->exprs.back()->expr_type;
      } else {
        expr->type = Type::kVoid;
      }
      return expr;
    }
    case WASM_COMMAND_TYPE_ASSERT_RETURN_NAN: {
      auto* expr = new TestScriptExpr(out_module_,
                                      TestScriptExpr::kAssertReturnNaN,
                                      command->assert_return_nan.action.loc);
      expr->invoke.reset(ConvertInvoke(command->assert_return_nan.action));
      expr->type = expr->invoke->type;
      return expr;
    }
    case WASM_COMMAND_TYPE_ASSERT_TRAP: {
      auto* expr = new TestScriptExpr(out_module_,
                                      TestScriptExpr::kAssertTrap,
                                      command->assert_trap.action.loc);
      expr->invoke.reset(ConvertInvoke(command->assert_trap.action));
      expr->trap_text.assign(command->assert_trap.text.start,
                             command->assert_trap.text.length);
      expr->type = expr->invoke->type;
      return expr;
    }
    default:
      assert(false);
  }
  return nullptr;
}

int Parser::ConvertAST(const WasmScript& script, bool spec_script_mode) {
  for (size_t i = 0; i < script.commands.size; ++i) {
    WasmCommand* command = &script.commands.data[i];
    if (command->type == WASM_COMMAND_TYPE_MODULE) {
      modules.emplace_back(ConvertModule(&command->module));
    } else {
      if (!spec_script_mode) {
	puts(
            "Spec invoke/assertion found in the script, but not in "
            "spec script mode.\n");
        return 1;
      }
      test_script.emplace_back(ConvertTestScriptExpr(command));
    }
  }
  return 0;
}

}  // namespace wasm
