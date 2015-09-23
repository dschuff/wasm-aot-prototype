#include "waot.h"
#include "wasm.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <cassert>

// Should I just give up and do 'using namespace llvm' like everything in LLVM?
using llvm::BasicBlock;
using llvm::Function;
using llvm::FunctionType;
using llvm::IRBuilder;
using llvm::Module;
using llvm::SmallVector;
using llvm::Type;
using llvm::Value;

static Type* getLLVMType(WasmType T, llvm::LLVMContext& C) {
  switch (T) {
    case WASM_TYPE_VOID:
      return Type::getVoidTy(C);
    case WASM_TYPE_I32:
      return Type::getInt32Ty(C);
    case WASM_TYPE_I64:
      return Type::getInt64Ty(C);
    case WASM_TYPE_F32:
      return Type::getFloatTy(C);
    case WASM_TYPE_F64:
      return Type::getDoubleTy(C);
    default:
      llvm_unreachable("Unexpexted type in getLLVMType");
  }
}

static std::string Mangle(const std::string& module,
                          const std::string& function) {
  return std::string("#" + module + "#" + function);
}

std::unique_ptr<Module> WAOTVisitor::VisitModule(const wasm::Module& mod) {
  module_ = llvm::make_unique<Module>(mod.name, ctx_);
  assert(module_ && "Could not create Module");

  for (auto& imp : mod.imports) {
    VisitImport(imp);
  }
  for (auto& func : mod.functions) {
    VisitFunction(func);
  }
  for (auto& exp : mod.exports) {
    VisitExport(exp);
  }
  return std::move(module_);
}

Function* WAOTVisitor::GetFunction(const wasm::Callable& func,
                                   Function::LinkageTypes linkage) {
  Type* ret_type = Type::getVoidTy(ctx_);
  if (func.result_type != WASM_TYPE_VOID) {
    ret_type = getLLVMType(func.result_type, ctx_);
  }
  SmallVector<Type*, 4> arg_types;
  for (auto& arg : func.args) {
    arg_types.push_back(getLLVMType(arg.type, ctx_));
  }

  auto* f = Function::Create(FunctionType::get(ret_type, arg_types, false),
                             linkage, func.local_name.c_str(), module_.get());
  assert(f && "Could not create Function");

  auto arg_iterator = f->arg_begin();
  for (auto& arg : func.args) {
    if (!arg.local_name.empty())
      arg_iterator->setName(arg.local_name);
    ++arg_iterator;
  }
  functions_.emplace(&func, f);
  return f;
}

void WAOTVisitor::VisitFunction(const wasm::Function& func) {
  auto* f = GetFunction(func, Function::InternalLinkage);

  BasicBlock::Create(ctx_, "entry", f);
  auto& bb = f->getEntryBlock();

  IRBuilder<> irb(&bb);
  for (auto& local : func.locals) {
    irb.CreateAlloca(getLLVMType(local.type, ctx_), nullptr,
                     local.local_name.c_str());
  }
  current_func_ = f;
  current_bb_ = &bb;

  Value* last_value = nullptr;
  for (auto& expr : func.body) {
    last_value = VisitExpression(*expr);
  }
  // Handle implicit return of the last expression
  if (!bb.getTerminator()) {
    if (func.result_type == WASM_TYPE_VOID) {
      irb.CreateRetVoid();
    } else {
      assert(func.body.size());
      irb.CreateRet(last_value);
    }
  }
}

void WAOTVisitor::VisitImport(const wasm::Import& imp) {
  auto* f = GetFunction(imp, Function::ExternalLinkage);
  f->setName(Mangle(imp.module_name, imp.func_name));
}

void WAOTVisitor::VisitExport(const wasm::Export& exp) {
  llvm::GlobalAlias::create(functions_[exp.function]->getType(),
                            Function::ExternalLinkage,
                            Mangle(exp.module->name, exp.name),
                            functions_[exp.function], module_.get());
}

void WAOTVisitor::VisitSegment(const wasm::Segment& seg) {}

Value* WAOTVisitor::VisitNop() {
  return nullptr;
}
Value* WAOTVisitor::VisitBlock(const wasm::Expression::ExprVector& exprs) {
  return nullptr;
}

Value* WAOTVisitor::VisitCall(WasmOpType opcode,
                              const wasm::Callable& callee,
                              int callee_index,
                              const wasm::Expression::ExprVector& args) {
  assert(current_bb_);
  BasicBlock* bb(current_bb_);
  SmallVector<Value*, 8> arg_values;
  for (auto& arg : args) {
    arg_values.push_back(VisitExpression(*arg));
  }
  IRBuilder<> irb(bb);
  return irb.CreateCall(functions_[&callee], arg_values);
}

Value* WAOTVisitor::VisitReturn(const wasm::Expression::ExprVector& value) {
  IRBuilder<> irb(current_bb_);
  if (!value.size())
    return irb.CreateRetVoid();
  return irb.CreateRet(VisitExpression(*value.front()));
}

Value* WAOTVisitor::VisitConst(const wasm::Literal& l) {
  switch (l.type) {
    case WASM_TYPE_VOID:
      return llvm::UndefValue::get(Type::getVoidTy(ctx_));
    case WASM_TYPE_I32:
    case WASM_TYPE_I64:
      return llvm::ConstantInt::get(
          getLLVMType(l.type, ctx_),
          l.type == WASM_TYPE_I32 ? l.value.i32 : l.value.i64);
    case WASM_TYPE_F32:
    case WASM_TYPE_F64:
      return llvm::ConstantFP::get(
          getLLVMType(l.type, ctx_),
          l.type == WASM_TYPE_F32 ? l.value.f32 : l.value.f64);
    default:
      assert(false);
  }
}
