#include "waot_visitor.h"
#include "ast_dumper.h"
#include "wart_trap.h"
#include "wasm.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>

// Should I just give up and do 'using namespace llvm' like everything in LLVM?
using llvm::BasicBlock;
using llvm::BranchInst;
using llvm::cast;
using llvm::CmpInst;
using llvm::Constant;
using llvm::ConstantInt;
using llvm::ConstantFP;
using llvm::Function;
using llvm::FunctionType;
using llvm::Instruction;
using llvm::IRBuilder;
using llvm::isa;
using llvm::Module;
using llvm::ReturnInst;
using llvm::SmallVector;
using llvm::TerminatorInst;
using llvm::Type;
using llvm::Value;

Type* WAOTVisitor::getLLVMType(wasm::Type T) {
  switch (T) {
    case wasm::Type::kVoid:
      return Type::getVoidTy(ctx_);
    case wasm::Type::kI32:
      return Type::getInt32Ty(ctx_);
    case wasm::Type::kI64:
      return Type::getInt64Ty(ctx_);
    case wasm::Type::kF32:
      return Type::getFloatTy(ctx_);
    case wasm::Type::kF64:
      return Type::getDoubleTy(ctx_);
    default:
      llvm_unreachable("Unexpexted type in getLLVMType");
  }
}

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
    default:
      return "(unknown type)";
  }
}

static std::string RuntimeFuncName(const char* name) {
  return std::string("__wasm_") + name;
}

static std::string RuntimeFuncName(const char* name, wasm::Type ty) {
  return RuntimeFuncName(name) + "_" + TypeName(ty);
}

static std::string Mangle(const std::string& module,
                          const std::string& function) {
  return std::string("." + module + "." + function);
}

static std::string CtorName(const wasm::Module& mod) {
  return std::string("." + mod.name + "_ctor");
}

static std::string DtorName(const wasm::Module& mod) {
  return std::string("." + mod.name + "_dtor");
}

static std::string MembaseGlobalName() {
  return std::string(".wasm_membase");
}

static std::string NumberedName(llvm::StringRef name, int number) {
  std::string name_str;
  llvm::raw_string_ostream name_stream(name_str);
  name_stream << name << "_" << number;
  name_stream.flush();
  return name_str;
}

Function* WAOTVisitor::CreateModuleConstructor(const wasm::Module& mod) {
  auto* f = Function::Create(
      cast<FunctionType>(
          cast<llvm::PointerType>(initfini_fn_ty_)->getElementType()),
      Function::InternalLinkage,
      CtorName(mod),
      module_);
  BasicBlock::Create(ctx_, "entry", f);
  irb_.SetInsertPoint(&f->getEntryBlock());
  const auto& DL(module_->getDataLayout());
  auto* membase = cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      MembaseGlobalName(),
      llvm::ArrayType::get(irb_.getInt8Ty(), 4ULL * 1024 * 1024 * 1024ULL)));
  membase->setSection(".membase");
  irb_.CreateCall(
      module_->getOrInsertFunction(
          RuntimeFuncName("init_memory"),
          // Assume size_t is the same size as void*
          FunctionType::get(
              irb_.getVoidTy(),
              {irb_.getInt8PtrTy(), DL.getIntPtrType(module_->getContext())},
              false)),
      {llvm::ConstantExpr::getBitCast(membase, irb_.getInt8PtrTy()),
       irb_.getIntN(DL.getPointerSizeInBits(), mod.initial_memory_size)});
  // Assume size_t is the same size as void*
  unsigned ptr_size = DL.getPointerSizeInBits();
  for (auto& seg : mod.segments) {
    auto* initializer = segment_initializers_[seg.get()];
    irb_.CreateCall(
        module_->getOrInsertFunction(
            RuntimeFuncName("init_segment"),
            FunctionType::get(
                irb_.getVoidTy(),
                {irb_.getInt8PtrTy(), DL.getIntPtrType(module_->getContext()),
                 DL.getIntPtrType(module_->getContext()),
                 initializer->getType()},
                false)),
        {llvm::ConstantExpr::getPointerCast(membase, irb_.getInt8PtrTy()),
         irb_.getIntN(ptr_size, seg->address),
         irb_.getIntN(ptr_size, seg->size), initializer});
  }
  irb_.CreateRetVoid();
  AddInitFunc(f);
  return f;
}

Function* WAOTVisitor::CreateModuleDestructor(const wasm::Module& mod) {
  auto* f = Function::Create(
      cast<FunctionType>(
          cast<llvm::PointerType>(initfini_fn_ty_)->getElementType()),
      Function::InternalLinkage,
      DtorName(mod),
      module_);
  BasicBlock::Create(ctx_, "entry", f);
  irb_.SetInsertPoint(&f->getEntryBlock());
  auto* membase =
      cast<llvm::GlobalVariable>(module_->getNamedGlobal(MembaseGlobalName()));
  irb_.CreateCall(
      module_->getOrInsertFunction(
          RuntimeFuncName("fini_memory"),
          FunctionType::get(irb_.getVoidTy(), {irb_.getInt8PtrTy()}, false)),
      {llvm::ConstantExpr::getPointerCast(membase, irb_.getInt8PtrTy())});

  irb_.CreateRetVoid();
  AddFiniFunc(f);
  return f;
}

Module* WAOTVisitor::VisitModule(const wasm::Module& mod) {
  assert(module_);
  current_wasm_module_ = &mod;
  for (auto& imp : mod.imports)
    VisitImport(*imp);
  for (auto& func : mod.functions)
    VisitFunction(*func);
  for (auto& exp : mod.exports)
    VisitExport(*exp);
  if (mod.initial_memory_size) {
    for (auto& seg : mod.segments)
      VisitSegment(*seg);
  }
  CreateModuleConstructor(mod);
  CreateModuleDestructor(mod);
  current_wasm_module_ = nullptr;
  return module_;
}

Function* WAOTVisitor::GetFunction(const wasm::Callable& func,
                                   Function::LinkageTypes linkage) {
  Type* ret_type = Type::getVoidTy(ctx_);
  if (func.result_type != wasm::Type::kVoid) {
    ret_type = getLLVMType(func.result_type);
  }
  SmallVector<Type*, 8> arg_types;
  for (auto& arg : func.args) {
    arg_types.push_back(getLLVMType(arg->type));
  }

  auto* f = Function::Create(FunctionType::get(ret_type, arg_types, false),
                             linkage,
                             func.local_name.c_str(),
                             module_);
  assert(f && "Could not create Function");

  auto arg_iterator = f->arg_begin();
  for (auto& arg : func.args) {
    if (!arg->local_name.empty())
      arg_iterator->setName(arg->local_name);
    ++arg_iterator;
  }
  functions_.emplace(&func, f);
  return f;
}

static Value* PromoteI1(Value* operand,
                        Type* to_type,
                        IRBuilder<>* irb,
                        llvm::LLVMContext& context) {
  if (operand && operand->getType() == Type::getInt1Ty(context))
    return irb->CreateZExt(operand, to_type);
  return operand;
}

void WAOTVisitor::VisitFunction(const wasm::Function& func) {
  auto* f = GetFunction(func, Function::InternalLinkage);
  current_func_ = f;

  BasicBlock::Create(ctx_, "entry", f);
  auto* bb = &f->getEntryBlock();
  irb_.SetInsertPoint(bb);

  unsigned i = 0;
  for (auto& local : func.locals) {
    current_locals_.push_back(
        irb_.CreateAlloca(getLLVMType(local->type), nullptr));
    if (!local->local_name.empty()) {
      current_locals_.back()->setName(local->local_name.c_str());
    } else if (i < func.args.size()) {
      current_locals_.back()->setName("arg");
    } else {
      current_locals_.back()->setName("local");
    }
    ++i;
  }
  i = 0;
  for (auto& arg : f->args()) {
    irb_.CreateStore(&arg, current_locals_[i++]);
  }

  Value* last_value = nullptr;
  for (auto& expr : func.body) {
    last_value = VisitExpression(expr.get());
  }
  // Handle implicit return of the last expression
  auto* current_bb = irb_.GetInsertBlock();
  if (!current_bb->getTerminator()) {
    if (func.result_type == wasm::Type::kVoid) {
      irb_.CreateRetVoid();
    } else {
      assert(func.body.size());
      irb_.CreateRet(
          PromoteI1(last_value, getLLVMType(func.result_type), &irb_, ctx_));
    }
  }
  current_func_ = nullptr;
  current_locals_.clear();
}

void WAOTVisitor::VisitImport(const wasm::Import& imp) {
  auto* f = GetFunction(imp, Function::ExternalLinkage);
  f->setName(Mangle(imp.module_name, imp.func_name));
}

void WAOTVisitor::VisitExport(const wasm::Export& exp) {
  llvm::GlobalAlias::create(functions_[exp.function]->getType(),
                            Function::ExternalLinkage,
                            Mangle(exp.module->name, exp.name),
                            functions_[exp.function],
                            module_);
}

void WAOTVisitor::VisitSegment(const wasm::Segment& seg) {
  auto* segment_data = new llvm::GlobalVariable(
      *module_,
      llvm::ArrayType::get(irb_.getInt8Ty(), seg.size),
      false,
      llvm::GlobalVariable::LinkageTypes::InternalLinkage,
      nullptr,
      Mangle(current_wasm_module_->name, NumberedName("segment", seg.address)));
  segment_data->setInitializer(
      llvm::ConstantDataArray::getString(ctx_, seg.as_string(), false));
  segment_initializers_[&seg] = segment_data;
}

Value* WAOTVisitor::VisitNop(wasm::Expression* expr) {
  return nullptr;
}
Value* WAOTVisitor::VisitBlock(wasm::Expression* expr,
                               wasm::UniquePtrVector<wasm::Expression>* exprs) {
  Value* ret = nullptr;  // A void expr instead?
  for (auto& expr : *exprs) {
    ret = VisitExpression(expr.get());
  }
  return ret;
}

static CmpInst::Predicate GetIntPredicate(wasm::CompareOperator relop) {
  switch (relop) {
    case wasm::kEq:
      return CmpInst::ICMP_EQ;
    case wasm::kNE:
      return CmpInst::ICMP_NE;
    case wasm::kLtS:
      return CmpInst::ICMP_SLT;
    case wasm::kLtU:
      return CmpInst::ICMP_ULT;
    case wasm::kLeS:
      return CmpInst::ICMP_SLE;
    case wasm::kLeU:
      return CmpInst::ICMP_ULE;
    case wasm::kGtS:
      return CmpInst::ICMP_SGT;
    case wasm::kGtU:
      return CmpInst::ICMP_UGT;
    case wasm::kGeS:
      return CmpInst::ICMP_SGE;
    case wasm::kGeU:
      return CmpInst::ICMP_UGE;
    default:
      assert(false && "Unexpected int compare op");
  }
}

static CmpInst::Predicate GetFPPredicate(wasm::CompareOperator relop) {
  switch (relop) {
    case wasm::kEq:
      return CmpInst::FCMP_OEQ;
    case wasm::kNE:
      return CmpInst::FCMP_UNE;
    case wasm::kLt:
      return CmpInst::FCMP_OLT;
    case wasm::kLe:
      return CmpInst::FCMP_OLE;
    case wasm::kGt:
      return CmpInst::FCMP_OGT;
    case wasm::kGe:
      return CmpInst::FCMP_OGE;
    default:
      assert(false && "Unexpected FP compare op");
  }
}

static Value* CreateCompare(Type* type,
                            wasm::CompareOperator relop,
                            IRBuilder<>* irb,
                            Value* lhs,
                            Value* rhs,
                            llvm::StringRef name) {
  Value* cmp_result;
  if (type->isIntOrIntVectorTy()) {
    cmp_result = irb->CreateICmp(GetIntPredicate(relop), lhs, rhs, name);
  } else if (lhs->getType()->isFloatTy() || lhs->getType()->isDoubleTy()) {
    cmp_result = irb->CreateFCmp(GetFPPredicate(relop), lhs, rhs, name);
  } else {
    assert(false);
  }
  return cmp_result;
}

Value* WAOTVisitor::VisitIf(wasm::Expression* expr,
                            wasm::Expression* condition,
                            wasm::Expression* then,
                            wasm::Expression* els) {
  Value* cmp_result = irb_.CreateICmpNE(VisitExpression(condition),
                                        ConstantInt::get(irb_.getInt32Ty(), 0),
                                        "if_cmp");

  auto* then_bb = BasicBlock::Create(ctx_, "if.then", current_func_);
  auto* else_bb = BasicBlock::Create(ctx_, "if.else", current_func_);
  auto* end_bb = BasicBlock::Create(ctx_, "if.end", current_func_);
  irb_.CreateCondBr(cmp_result, then_bb, else_bb);
  irb_.SetInsertPoint(then_bb);
  Value* then_expr = VisitExpression(then);
  if (then_expr && isa<TerminatorInst>(then_expr)) {
    assert(isa<ReturnInst>(then_expr));
  } else {
    irb_.CreateBr(end_bb);
  }
  then_bb = irb_.GetInsertBlock();

  Value* else_expr = nullptr;
  irb_.SetInsertPoint(else_bb);
  if (els) {
    else_expr = VisitExpression(els);
    if (else_expr && isa<TerminatorInst>(else_expr)) {
      assert(isa<ReturnInst>(else_expr));
    } else {
      irb_.CreateBr(end_bb);
    }
  } else {
    irb_.CreateBr(end_bb);
  }
  else_bb = irb_.GetInsertBlock();

  Value* ret = nullptr;
  if (expr->expected_type != wasm::Type::kVoid) {
    Type* expr_type = getLLVMType(expr->expected_type);
    irb_.SetInsertPoint(end_bb);
    if (!isa<TerminatorInst>(then_expr) ||
        (else_expr && !isa<TerminatorInst>(else_expr))) {
      auto* phi = irb_.CreatePHI(expr_type, 2, "if.result");
      if (then_expr && !isa<TerminatorInst>(then_expr)) {
        phi->addIncoming(then_expr, then_bb);
      } else {
        phi->addIncoming(llvm::UndefValue::get(expr_type), then_bb);
      }
      if (else_expr && !isa<TerminatorInst>(else_expr)) {
        phi->addIncoming(else_expr, else_bb);
      } else {
        phi->addIncoming(llvm::UndefValue::get(expr_type), else_bb);
      }
      ret = phi;
    } else {
      irb_.CreateUnreachable();
    }
  }
  irb_.SetInsertPoint(end_bb);

  return ret;
}

Value* WAOTVisitor::VisitCall(wasm::CallExpression* expr,
                              bool is_import,
                              wasm::Callable* callee,
                              int callee_index,
                              wasm::UniquePtrVector<wasm::Expression>* args) {
  SmallVector<Value*, 8> arg_values;
  for (auto& arg : *args) {
    arg_values.push_back(VisitExpression(arg.get()));
  }
  return irb_.CreateCall(functions_[callee], arg_values);
}

Value* WAOTVisitor::VisitReturn(
    wasm::Expression* expr,
    wasm::UniquePtrVector<wasm::Expression>* value) {
  Value* retval = nullptr;
  if (value->size())
    retval = VisitExpression(value->front().get());
  return irb_.CreateRet(
      PromoteI1(retval, current_func_->getReturnType(), &irb_, ctx_));
}

Value* WAOTVisitor::VisitGetLocal(wasm::LocalExpression* expr,
                                  wasm::Variable* var) {
  auto* load_addr = current_locals_[var->index];
  return irb_.CreateLoad(getLLVMType(var->type), load_addr, "get_local");
}

Value* WAOTVisitor::VisitSetLocal(wasm::LocalExpression* expr,
                                  wasm::Variable* var,
                                  wasm::Expression* value) {
  Value* store_addr = current_locals_[var->index];
  auto* store_value = VisitExpression(value);
  return irb_.CreateStore(store_value, store_addr);
}

Value* WAOTVisitor::VisitMemory(wasm::Expression* expr,
                                wasm::MemoryOperator memop,
                                wasm::MemType mem_type,
                                uint32_t mem_alignment,
                                uint64_t mem_offset,
                                bool is_signed,
                                wasm::Expression* address,
                                wasm::Expression* store_expr) {
  Value* address_val = VisitExpression(address);
  Value* store_val = nullptr;
  if (store_expr)
    store_val = VisitExpression(store_expr);
  // get the membase
  auto* membase_var = cast<llvm::GlobalVariable>(module_->getOrInsertGlobal(
      MembaseGlobalName(),
      llvm::ArrayType::get(irb_.getInt8Ty(), 4ULL * 1024 * 1024 * 1024ULL)));
  auto* membase =
      llvm::ConstantExpr::getPointerCast(membase_var, irb_.getInt8PtrTy());
  // TODO: check offset
  // TODO: wasm64
  auto* offset = irb_.getInt32(mem_offset);
  // TODO: infinite precision
  // addr = getelementptr(membase, address_val) -> getelementptr(membase,
  // address_val+offset)
  auto* index = irb_.CreateAdd(address_val, offset, "effective_addr");
  auto* addr = irb_.CreateGEP(membase, index);
  // TODO: alignment

  if (store_expr) {
    auto* store_val_trunc =
        mem_type.IsFloatTy()
            ? store_val
            : irb_.CreateTrunc(store_val,
                               irb_.getIntNTy(mem_type.GetSizeInBits()),
                               "storetype_val");
    irb_.CreateStore(store_val_trunc,
                     irb_.CreatePointerCast(
                         addr, store_val_trunc->getType()->getPointerTo()));
    return nullptr;
  } else {
    auto* expr_type = getLLVMType(expr->expr_type);
    auto* load_type = mem_type.IsFloatTy()
                          ? expr_type
                          : irb_.getIntNTy(mem_type.GetSizeInBits());

    auto* loaded_val =
        irb_.CreateLoad(irb_.CreatePointerCast(addr, load_type->getPointerTo()),
                        "loadtype_val");
    return is_signed ? irb_.CreateSExt(loaded_val, expr_type, "load_val")
                     : irb_.CreateZExt(loaded_val, expr_type, "load_val");
  }
}

Value* WAOTVisitor::VisitConst(wasm::Expression* expr, wasm::Literal* l) {
  switch (l->type) {
    case wasm::Type::kVoid:
      return llvm::UndefValue::get(Type::getVoidTy(ctx_));
    case wasm::Type::kI32:
    case wasm::Type::kI64:
      return ConstantInt::get(
          getLLVMType(l->type),
          l->type == wasm::Type::kI32 ? l->value.i32 : l->value.i64);
    case wasm::Type::kF32:
    case wasm::Type::kF64:
      return ConstantFP::get(
          getLLVMType(l->type),
          l->type == wasm::Type::kF32 ? l->value.f32 : l->value.f64);
    default:
      assert(false);
  }
}

static const llvm::Intrinsic::ID GetUnaryOpIntrinsic(wasm::UnaryOperator unop) {
  switch (unop) {
    case wasm::kClz:
      return llvm::Intrinsic::ctlz;
    case wasm::kCtz:
      return llvm::Intrinsic::cttz;
    case wasm::kPopcnt:
      return llvm::Intrinsic::ctpop;
    case wasm::kAbs:
      return llvm::Intrinsic::fabs;
    case wasm::kCeil:
      return llvm::Intrinsic::ceil;
    case wasm::kFloor:
      return llvm::Intrinsic::floor;
    case wasm::kTrunc:
      return llvm::Intrinsic::trunc;
    case wasm::kNearest:
      return llvm::Intrinsic::rint;
    case wasm::kSqrt:
      return llvm::Intrinsic::sqrt;
    default:
      assert(false && "Unexpected operand in GetUnaryOpIntrinsic");
  }
}

Value* WAOTVisitor::VisitUnop(wasm::Expression* expr,
                              wasm::UnaryOperator unop,
                              wasm::Expression* operand) {
  assert(operand->expr_type == expr->expr_type);
  Value* op = VisitExpression(operand);
  // FNeg is represented as fsub float -0.0, %val
  if (unop == wasm::kNeg) {
    return irb_.CreateFSub(ConstantFP::get(getLLVMType(expr->expr_type), -0.0),
                           op);
  }
  Function* intrin = llvm::Intrinsic::getDeclaration(
      module_, GetUnaryOpIntrinsic(unop), getLLVMType(expr->expr_type));
  SmallVector<Value*, 2> args;
  args.push_back(op);
  // For Clt/Ctz, we want 0 to produce a defined result.
  if (unop == wasm::kClz || unop == wasm::kCtz)
    args.push_back(ConstantInt::getFalse(Type::getInt1Ty(ctx_)));
  return irb_.CreateCall(intrin, args, "unop_expr");
}

static Instruction::BinaryOps GetBinopOpcode(wasm::Type type,
                                             wasm::BinaryOperator binop) {
  switch (binop) {
    case wasm::kAdd:
      return type <= wasm::Type::kI64 ? Instruction::BinaryOps::Add
                                      : Instruction::BinaryOps::FAdd;
    case wasm::kSub:
      return type <= wasm::Type::kI64 ? Instruction::BinaryOps::Sub
                                      : Instruction::BinaryOps::FSub;
    case wasm::kMul:
      return type <= wasm::Type::kI64 ? Instruction::BinaryOps::Mul
                                      : Instruction::BinaryOps::FMul;
    case wasm::kDivS:
      return Instruction::BinaryOps::SDiv;
    case wasm::kDivU:
      return Instruction::BinaryOps::UDiv;
    case wasm::kRemS:
      return Instruction::BinaryOps::SRem;
    case wasm::kRemU:
      return Instruction::BinaryOps::URem;
    case wasm::kAnd:
      return Instruction::BinaryOps::And;
    case wasm::kOr:
      return Instruction::BinaryOps::Or;
    case wasm::kXor:
      return Instruction::BinaryOps::Xor;
    case wasm::kShl:
      return Instruction::BinaryOps::Shl;
    case wasm::kShrU:
      return Instruction::BinaryOps::LShr;
    case wasm::kShrS:
      return Instruction::BinaryOps::AShr;
    case wasm::kDiv:
      return Instruction::BinaryOps::FDiv;
    default:
      return Instruction::BinaryOps::BinaryOpsEnd;
  }
}

static Value* VisitShift(Instruction::BinaryOps opcode,
                         Value* lhs,
                         Value* rhs,
                         IRBuilder<>* irb) {
  auto* op_ty = cast<llvm::IntegerType>(lhs->getType());
  unsigned op_width = op_ty->getIntegerBitWidth();
  Value* shiftop_check =
      irb->CreateICmpUGE(rhs, ConstantInt::get(op_ty, op_width), "shamt_check");

  if (opcode == Instruction::BinaryOps::AShr) {
    // If the shift amount is >= the type size, the result must be 0 or -1. This
    // is equivalent to shift of type size - 1 bits.
    Value* shift_amt = irb->CreateSelect(
        shiftop_check, ConstantInt::get(op_ty, op_width - 1), rhs, "shamt");
    return irb->CreateBinOp(opcode, lhs, shift_amt, "shift_expr");
  } else {
    // If the shift amount is >= the type size, the result must be 0. To avoid a
    // branch, execute the shift and select on the shift amount. LLVM langref
    // says that the *result* of the shift is undefined if the rhs is too large,
    // which I *think* means we're ok (i.e. bogus value but no nasal demons).
    // Practically I don't know of any architecture where it would trap or
    // anything strange.
    Value* shift_result = irb->CreateBinOp(opcode, lhs, rhs, "shift_result");
    return irb->CreateSelect(
        shiftop_check, ConstantInt::get(op_ty, 0), shift_result, "shift_expr");
  }
}

static Constant* GetTrapFunction(Module* module) {
  return module->getOrInsertFunction(
      RuntimeFuncName("trap"),
      FunctionType::get(Type::getVoidTy(module->getContext()),
                        {Type::getInt32Ty(module->getContext())},
                        false));
}

Value* WAOTVisitor::VisitIDiv(Instruction::BinaryOps opcode,
                              Value* lhs,
                              Value* rhs) {
  Value* cmp_result = irb_.CreateICmpEQ(
      rhs, ConstantInt::get(rhs->getType(), 0), "divzero_check");
  auto* next_bb = BasicBlock::Create(ctx_, "div.next", current_func_);
  auto* trap_bb = BasicBlock::Create(ctx_, "div.trap", current_func_);
  irb_.CreateCondBr(cmp_result, trap_bb, next_bb);
  irb_.SetInsertPoint(trap_bb);
  irb_.CreateCall(
      GetTrapFunction(module_),
      {ConstantInt::get(Type::getInt32Ty(ctx_), wart::kIntegerDivideByZero)});
  irb_.CreateUnreachable();

  irb_.SetInsertPoint(next_bb);
  return irb_.CreateBinOp(opcode, lhs, rhs);
}

Constant* WAOTVisitor::GetBinaryOpCallee(wasm::Type wasm_ty,
                                         wasm::BinaryOperator binop) {
  Type* expr_ty = getLLVMType(wasm_ty);
  switch (binop) {
    case wasm::kCopySign:
      return llvm::Intrinsic::getDeclaration(
          module_, llvm::Intrinsic::copysign, expr_ty);
    case wasm::kMin:
      return module_->getOrInsertFunction(
          RuntimeFuncName("float_min", wasm_ty),
          FunctionType::get(expr_ty, {expr_ty, expr_ty}, false));
    case wasm::kMax:
      return module_->getOrInsertFunction(
          RuntimeFuncName("float_max", wasm_ty),
          FunctionType::get(expr_ty, {expr_ty, expr_ty}, false));
    default:
      llvm_unreachable("Unexpected operator in GetBinaryOpIntrinsic");
  }
}

Value* WAOTVisitor::VisitCallBinop(wasm::Type wasm_ty,
                                   wasm::BinaryOperator binop,
                                   Value* lhs,
                                   Value* rhs,
                                   IRBuilder<>* current_irb) {
  Constant* callee = GetBinaryOpCallee(wasm_ty, binop);
  return current_irb->CreateCall(callee, {lhs, rhs});
}

Value* WAOTVisitor::VisitBinop(wasm::Expression* expr,
                               wasm::BinaryOperator binop,
                               wasm::Expression* lhs,
                               wasm::Expression* rhs) {
  Instruction::BinaryOps opcode = GetBinopOpcode(expr->expr_type, binop);
  Value* lhs_value = VisitExpression(lhs);
  Value* rhs_value = VisitExpression(rhs);
  switch (binop) {
    case wasm::kShl:
    case wasm::kShrU:
    case wasm::kShrS:
      return VisitShift(opcode, lhs_value, rhs_value, &irb_);
    case wasm::kDivS:
    case wasm::kDivU:
    case wasm::kRemS:
    case wasm::kRemU:
      return VisitIDiv(opcode, lhs_value, rhs_value);
    case wasm::kCopySign:
    case wasm::kMin:
    case wasm::kMax:
      return VisitCallBinop(
          expr->expr_type, binop, lhs_value, rhs_value, &irb_);
    default:
      break;
  }
  return irb_.CreateBinOp(opcode, lhs_value, rhs_value);
}

Value* WAOTVisitor::VisitCompare(wasm::Expression* expr,
                                 wasm::Type compare_type,
                                 wasm::CompareOperator relop,
                                 wasm::Expression* lhs,
                                 wasm::Expression* rhs) {
  Value* lhs_val = VisitExpression(lhs);
  Value* rhs_val = VisitExpression(rhs);
  return CreateCompare(getLLVMType(compare_type),
                       relop,
                       &irb_,
                       lhs_val,
                       rhs_val,
                       "compare_epxr");
}

void WAOTVisitor::TrapIfNaN(Value* lhs) {
  Value* cmp = irb_.CreateFCmp(CmpInst::FCMP_ORD, lhs, lhs, "nan_check");
  auto* next_bb = BasicBlock::Create(ctx_, "conv.next", current_func_);
  auto* trap_bb = BasicBlock::Create(ctx_, "conv.trap", current_func_);
  irb_.CreateCondBr(cmp, next_bb, trap_bb);
  irb_.SetInsertPoint(trap_bb);
  irb_.CreateCall(GetTrapFunction(module_),
                  {ConstantInt::get(Type::getInt32Ty(ctx_),
                                    wart::kInvalidConversionToInteger)});
  irb_.CreateUnreachable();
  irb_.SetInsertPoint(next_bb);
}

void WAOTVisitor::ToIntRangeCheck(Value* operand,
                                  Type* dest_type,
                                  bool is_signed) {
  double maxval, minval;
  CmpInst::Predicate min_pred = CmpInst::FCMP_ULT;
  minval = llvm::APInt::getSignedMinValue(dest_type->getIntegerBitWidth())
               .signedRoundToDouble();
  maxval = -minval;
  if (!is_signed) {
    maxval = -minval * 2.0;
    minval = -1.0;
    min_pred = CmpInst::FCMP_ULE;
  }

  Value* cmp1 = irb_.CreateFCmp(CmpInst::FCMP_UGE,
                                operand,
                                ConstantFP::get(operand->getType(), maxval),
                                "ovf_upper");
  Value* cmp2 = irb_.CreateFCmp(min_pred,
                                operand,
                                ConstantFP::get(operand->getType(), minval),
                                "ovf_lower");
  Value* should_trap = irb_.CreateOr(cmp1, cmp2, "overflow_check");
  auto* next_bb = BasicBlock::Create(ctx_, "conv.next", current_func_);
  auto* trap_bb = BasicBlock::Create(ctx_, "conv.trap", current_func_);
  irb_.CreateCondBr(should_trap, trap_bb, next_bb);
  irb_.SetInsertPoint(trap_bb);
  irb_.CreateCall(
      GetTrapFunction(module_),
      {ConstantInt::get(irb_.getInt32Ty(), wart::kIntegerOverflow)});
  irb_.CreateUnreachable();
  irb_.SetInsertPoint(next_bb);
}

Value* WAOTVisitor::VisitConversion(wasm::ConversionExpression* expr,
                                    wasm::ConversionOperator cvt,
                                    wasm::Expression* operand) {
  Value* operand_val = VisitExpression(operand);
  Type* result_ty = getLLVMType(expr->expr_type);
  const char* name = wasm::ConversionOpName(cvt);
  switch (cvt) {
    case wasm::kExtendSInt32:
      return irb_.CreateSExt(operand_val, result_ty, name);
    case wasm::kExtendUInt32:
      return irb_.CreateZExt(operand_val, result_ty, name);
    case wasm::kWrapInt64:
      return irb_.CreateTrunc(operand_val, result_ty, name);
    case wasm::kTruncSFloat32:
    case wasm::kTruncSFloat64:
      TrapIfNaN(operand_val);
      ToIntRangeCheck(operand_val, result_ty, true);
      return irb_.CreateFPToSI(operand_val, result_ty, name);
    case wasm::kTruncUFloat32:
    case wasm::kTruncUFloat64:
      TrapIfNaN(operand_val);
      ToIntRangeCheck(operand_val, result_ty, false);
      return irb_.CreateFPToUI(operand_val, result_ty, name);
    case wasm::kReinterpretFloat:
    case wasm::kReinterpretInt:
      return irb_.CreateBitCast(operand_val, result_ty, name);
    case wasm::kConvertSInt32:
    case wasm::kConvertSInt64:
      return irb_.CreateSIToFP(operand_val, result_ty, name);
    case wasm::kConvertUInt32:
    case wasm::kConvertUInt64:
      return irb_.CreateUIToFP(operand_val, result_ty, name);
    case wasm::kPromoteFloat32:
      return irb_.CreateFPExt(operand_val, result_ty, name);
    case wasm::kDemoteFloat64:
      return irb_.CreateFPTrunc(operand_val, result_ty, name);
    default:
      llvm_unreachable("Unexpected operator in VisitConversion");
  }
}

Value* WAOTVisitor::VisitInvoke(wasm::TestScriptExpr* expr,
                                wasm::Export* callee,
                                wasm::UniquePtrVector<wasm::Expression>* args) {
  // Generate a function @Invoke which calls the invoke's callee (and
  // evaluates its arguments which are wasm exprs). By default it returns the
  // result of the call, but if the invoke's type has been set to void, (e.g.
  // by AssertTrap) we discard it. Return the generated function.
  // @Invoke takes no arguments and returns void or the return type of the
  // callee.
  auto* ret_type = getLLVMType(expr->type);
  auto* f = Function::Create(FunctionType::get(ret_type, {}, false),
                             Function::ExternalLinkage,
                             NumberedName("Invoke", expr->source_loc.line),
                             module_);
  assert(f);
  BasicBlock::Create(ctx_, "entry", f);
  auto* bb = &f->getEntryBlock();
  irb_.SetInsertPoint(bb);
  Function* previous_func = current_func_;
  current_func_ = f;
  Value* call = VisitCall(nullptr,
                          false,
                          callee->function,
                          callee->function->index_in_module,
                          args);

  irb_.SetInsertPoint(bb);
  if (ret_type->isVoidTy()) {
    irb_.CreateRetVoid();
  } else {
    irb_.CreateRet(call);
  }
  if (!previous_func)
    AddInitFunc(f);
  current_func_ = nullptr;
  return f;
}

Value* WAOTVisitor::VisitAssertReturn(
    wasm::TestScriptExpr* expr,
    wasm::TestScriptExpr* invoke,
    wasm::UniquePtrVector<wasm::Expression>* expected) {
  // Generate a function @AssertReturn which calls the @Invoke function (which
  // was returned by VisitInvoke), evaluates the expectation expression,
  // compares the results, and calls a runtime function if they do not match.
  // @AssertReturn has no arguments and returns void. There is one assertion
  // failure handler __wasm_assert_fail_<type> for each wasm type, which
  // returns void and takes the assertion number and expected and actual result
  // values.
  auto* f =
      Function::Create(FunctionType::get(Type::getVoidTy(ctx_), {}, false),
                       Function::ExternalLinkage,
                       NumberedName("AssertReturn", expr->source_loc.line),
                       module_);
  BasicBlock::Create(ctx_, "entry", f);
  auto* bb = &f->getEntryBlock();
  irb_.SetInsertPoint(bb);
  assert(current_func_ == nullptr);
  current_func_ = f;

  Value* invoke_func = VisitInvoke(invoke, invoke->callee, &invoke->exprs);

  irb_.SetInsertPoint(bb);
  Value* result = irb_.CreateCall(invoke_func, {});
  // TODO: simplify this, now that only const exprs will be allowed?
  if (expected->size()) {
    Value* expected_result = VisitExpression(expected->front().get());
    assert(result->getType() == expected_result->getType());
    Value* casted_expectation = irb_.CreateBitCast(
        expected_result,
        irb_.getIntNTy(expected_result->getType()->getPrimitiveSizeInBits()),
        "expected_result_int");
    Value* casted_result = irb_.CreateBitCast(
        result, casted_expectation->getType(), "invoke_result_int");

    Value* cmp_result =
        irb_.CreateICmpEQ(casted_result, casted_expectation, "assert_check");

    BasicBlock* success_bb = BasicBlock::Create(ctx_, "AssertSuccess", f);
    llvm::ReturnInst::Create(ctx_, nullptr, success_bb);

    BasicBlock* fail_bb = BasicBlock::Create(ctx_, "AssertFail", f);
    irb_.CreateCondBr(cmp_result, success_bb, fail_bb);
    irb_.SetInsertPoint(fail_bb);

    // Call a runtime function, passing it the assertion line number, the type,
    // and the expected and actual values.
    Type* expected_type = getLLVMType(expected->front()->expr_type);
    irb_.CreateCall(
        module_->getOrInsertFunction(
            RuntimeFuncName("assert_fail", expected->front()->expr_type),
            FunctionType::get(
                Type::getVoidTy(ctx_),
                {Type::getInt32Ty(ctx_), expected_type, expected_type},
                false)),
        {ConstantInt::get(Type::getInt32Ty(ctx_), expr->source_loc.line),
         expected_result, result});
  }

  irb_.CreateRetVoid();
  AddInitFunc(f);
  current_func_ = nullptr;
  return f;
}

Value* WAOTVisitor::VisitAssertReturnNaN(wasm::TestScriptExpr* expr,
                                         wasm::TestScriptExpr* invoke) {
  auto* f =
      Function::Create(FunctionType::get(Type::getVoidTy(ctx_), {}, false),
                       Function::ExternalLinkage,
                       NumberedName("AssertReturnNaN", expr->source_loc.line),
                       module_);
  BasicBlock::Create(ctx_, "entry", f);
  auto* bb = &f->getEntryBlock();
  irb_.SetInsertPoint(bb);
  assert(current_func_ == nullptr);
  current_func_ = f;

  Type* i32_ty = Type::getInt32Ty(ctx_);
  Value* invoke_func = VisitInvoke(invoke, invoke->callee, &invoke->exprs);

  irb_.SetInsertPoint(bb);
  Value* result = irb_.CreateCall(invoke_func, {});
  // For assert_return_nan the runtime function also does the check, and does
  // the appropriate thing on failure.
  Constant* assert_func = module_->getOrInsertFunction(
      RuntimeFuncName("assert_return_nan", expr->type),
      FunctionType::get(
          Type::getVoidTy(ctx_), {i32_ty, getLLVMType(expr->type)}, false));
  irb_.CreateCall(assert_func,
                  {ConstantInt::get(i32_ty, expr->source_loc.line), result});
  irb_.CreateRetVoid();
  AddInitFunc(f);
  current_func_ = nullptr;
  return f;
}

Value* WAOTVisitor::VisitAssertTrap(wasm::TestScriptExpr* expr,
                                    wasm::TestScriptExpr* invoke,
                                    const std::string& text) {
  auto* f =
      Function::Create(FunctionType::get(Type::getVoidTy(ctx_), {}, false),
                       Function::ExternalLinkage,
                       NumberedName("AssertTrap", expr->source_loc.line),
                       module_);
  BasicBlock::Create(ctx_, "entry", f);
  auto* bb = &f->getEntryBlock();
  assert(current_func_ == nullptr);
  current_func_ = f;
  irb_.SetInsertPoint(bb);
  Type* i32_ty = Type::getInt32Ty(ctx_);

  // Set the invoke's type so VisitInvoke will return a void function
  invoke->type = wasm::Type::kVoid;
  Value* invoke_func = VisitInvoke(invoke, invoke->callee, &invoke->exprs);

  irb_.SetInsertPoint(bb);
  // Call a runtime function which sets up a trap handler and calls the invoke.
  FunctionType* assert_trap_func_type = FunctionType::get(
      Type::getVoidTy(ctx_), {i32_ty, invoke_func->getType()}, false);
  auto* assert_trap_func = module_->getOrInsertFunction(
      RuntimeFuncName("assert_trap"), assert_trap_func_type);
  irb_.CreateCall(
      assert_trap_func,
      {ConstantInt::get(i32_ty, expr->source_loc.line), invoke_func});

  irb_.CreateRetVoid();
  AddInitFunc(f);
  current_func_ = nullptr;
  return f;
}

void WAOTVisitor::FinishLLVMModule() {
  auto* array_delimiter =
      llvm::ConstantPointerNull::get(cast<llvm::PointerType>(initfini_fn_ty_));
  init_funcs_.push_back(array_delimiter);
  fini_funcs_.push_back(array_delimiter);

  auto* init_array = new llvm::GlobalVariable(
      *module_,
      llvm::ArrayType::get(initfini_fn_ty_, init_funcs_.size()),
      false,
      llvm::GlobalVariable::LinkageTypes::AppendingLinkage,
      nullptr,
      "__wasm_init_array");
  init_array->setInitializer(llvm::ConstantArray::get(
      cast<llvm::ArrayType>(init_array->getValueType()), init_funcs_));
  auto* fini_array = new llvm::GlobalVariable(
      *module_,
      llvm::ArrayType::get(initfini_fn_ty_, fini_funcs_.size()),
      false,
      llvm::GlobalVariable::LinkageTypes::AppendingLinkage,
      nullptr,
      "__wasm_fini_array");
  fini_array->setInitializer(llvm::ConstantArray::get(
      cast<llvm::ArrayType>(fini_array->getValueType()), fini_funcs_));
}
