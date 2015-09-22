#include "waot.h"
#include "wasm.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"


// Should I just give up and do 'using namespace llvm' like everything in LLVM?
using llvm::BasicBlock;
using llvm::Function;
using llvm::FunctionType;
using llvm::IRBuilder;
using llvm::Module;
using llvm::SmallVector;
using llvm::Type;

static Type *getLLVMType(WasmType T, llvm::LLVMContext &C) {
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

//WAOTVisitor::WAOTVisitor() {
//
//}

std::unique_ptr<Module>
WAOTVisitor::VisitModule(const wasm::Module& mod) {
  module_ = llvm::make_unique<Module>("wasm_module", ctx_);
  assert(module_ && "Could not create Module");

  for (auto& func : mod.functions) {
    VisitFunction(func);
  }
  return std::move(module_);
}

void WAOTVisitor::VisitFunction(const wasm::Function& func) {
  Type* ret_type = Type::getVoidTy(ctx_);
  if (func.result_type != WASM_TYPE_VOID) {
    ret_type = getLLVMType(func.result_type, ctx_);
  }
  SmallVector<Type*, 4> arg_types;
  for (auto& arg : func.args) {
    arg_types.push_back(getLLVMType(arg.type, ctx_));
  }

  auto *f = Function::Create(
  FunctionType::get(ret_type, arg_types, false),
    Function::InternalLinkage,
    func.local_name.c_str(), module_.get());
  assert(f && "Could not create Function");

  BasicBlock::Create(ctx_, "entry", f);
  auto &bb = f->getEntryBlock();

  IRBuilder<> irb(&bb);
  for (auto& local : func.locals) {
    irb.CreateAlloca(getLLVMType(local.type, ctx_),
                       nullptr,
                       local.local_name.c_str());
  }
}

void WAOTVisitor::VisitImport(const wasm::Import& imp) {}
void WAOTVisitor::VisitSegment(const wasm::Segment& seg) {}

AstValue WAOTVisitor::VisitNop() { return {};}
AstValue WAOTVisitor::VisitBlock(const wasm::Expression::ExprVector& exprs) { return {}; }
AstValue WAOTVisitor::VisitCall(WasmOpType opcode,
                                const wasm::Callable& callee,
                                int callee_index,
                                  const wasm::Expression::ExprVector& args) {return {}; }
AstValue WAOTVisitor::VisitReturn(const wasm::Expression::ExprVector& value) { return {}; }
AstValue WAOTVisitor::VisitConst(const wasm::Literal& l) { return {}; }
