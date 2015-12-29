#include "ast_visitor.h"
#include "wasm.h"
#include "wasm_ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <unordered_map>

namespace llvm {
class BasicBlock;
class Function;
class GlobalVariable;
}

class WAOTVisitor : public wasm::AstVisitor<llvm::Module*, llvm::Value*> {
 public:
  WAOTVisitor(llvm::Module* llvm_module)
      : module_(llvm_module),
        ctx_(llvm_module->getContext()),
        initfini_fn_ty_(llvm::FunctionType::get(
                            llvm::Type::getVoidTy(llvm_module->getContext()),
                            {},
                            false)
                            ->getPointerTo()),
        irb_(llvm_module->getContext()) {}

  bool SetEntryExport(const wasm::Callable* func) {
    llvm::Constant* start = functions_.at(func);
    if (start->getType() != initfini_fn_ty_) {
      return false;
    }
    AddInitFunc(llvm::cast<llvm::Function>(start));
    return true;
  }
  void FinishLLVMModule();

 protected:
  llvm::Module* VisitModule(const wasm::Module& mod) override;
  void VisitImport(const wasm::Import& imp) override;
  void VisitExport(const wasm::Export& exp) override;
  void VisitFunction(const wasm::Function& func) override;
  void VisitSegment(const wasm::Segment& seg) override;

  llvm::Value* VisitNop(wasm::Expression* expr) override;
  llvm::Value* VisitBlock(
      wasm::Expression* expr,
      wasm::UniquePtrVector<wasm::Expression>* exprs) override;
  llvm::Value* VisitIf(wasm::Expression* expr,
                       wasm::Expression* condition,
                       wasm::Expression* then,
                       wasm::Expression* els) override;
  llvm::Value* VisitCall(
      wasm::CallExpression* expr,
      bool is_import,
      wasm::Callable* callee,
      int callee_index,
      wasm::UniquePtrVector<wasm::Expression>* args) override;
  llvm::Value* VisitReturn(
      wasm::Expression* expr,
      wasm::UniquePtrVector<wasm::Expression>* value) override;
  llvm::Value* VisitGetLocal(wasm::LocalExpression* expr,
                             wasm::Variable* var) override;
  llvm::Value* VisitSetLocal(wasm::LocalExpression* expr,
                             wasm::Variable* var,
                             wasm::Expression* value) override;
  llvm::Value* VisitMemory(wasm::Expression* expr,
			   wasm::MemoryOperator memop,
			   wasm::MemType mem_type,
			   uint32_t mem_alignment,
			   uint64_t mem_offset,
			   bool is_signed,
			   wasm::Expression* address,
			   wasm::Expression* store_val) override;
  llvm::Value* VisitConst(wasm::Expression* expr, wasm::Literal* l) override;
  llvm::Value* VisitUnop(wasm::Expression* expr,
                         wasm::UnaryOperator unop,
                         wasm::Expression* operand) override;
  llvm::Value* VisitBinop(wasm::Expression* epxr,
                          wasm::BinaryOperator binop,
                          wasm::Expression* lhs,
                          wasm::Expression* rhs) override;
  llvm::Value* VisitCompare(wasm::Expression* expr,
                            wasm::Type compare_type,
                            wasm::CompareOperator relop,
                            wasm::Expression* lhs,
                            wasm::Expression* rhs) override;
  llvm::Value* VisitConversion(wasm::ConversionExpression* expr,
                               wasm::ConversionOperator cvt,
                               wasm::Expression* operand) override;

  llvm::Value* VisitInvoke(wasm::TestScriptExpr* expr,
                           wasm::Export* callee,
                           wasm::UniquePtrVector<wasm::Expression>*) override;
  llvm::Value* VisitAssertReturn(
      wasm::TestScriptExpr* expr,
      wasm::TestScriptExpr* arg,
      wasm::UniquePtrVector<wasm::Expression>* expected) override;
  llvm::Value* VisitAssertReturnNaN(wasm::TestScriptExpr* expr,
                                    wasm::TestScriptExpr* arg) override;
  llvm::Value* VisitAssertTrap(wasm::TestScriptExpr* expr,
                               wasm::TestScriptExpr* invoke) override;

 private:
  llvm::Function* GetFunction(const wasm::Callable& func,
                              llvm::Function::LinkageTypes linkage);
  llvm::Type* getLLVMType(wasm::Type ty);
  llvm::Function* CreateModuleConstructor(const wasm::Module& mod);
  llvm::Function* CreateModuleDestructor(const wasm::Module& mod);
  llvm::Constant* getAssertFailFunc(wasm::Type ty);
  llvm::Value* VisitIDiv(llvm::Instruction::BinaryOps opcode,
                         llvm::Value* lhs,
                         llvm::Value* rhs);
  void TrapIfNaN(llvm::Value* operand);
  void ToIntRangeCheck(llvm::Value* operand,
                       llvm::Type* dest_type,
                       bool is_signed);
  llvm::Constant* GetBinaryOpCallee(wasm::Type wasm_ty,
                                    wasm::BinaryOperator binop);
  llvm::Value* VisitCallBinop(wasm::Type wasm_ty,
                              wasm::BinaryOperator binop,
                              llvm::Value* lhs,
                              llvm::Value* rhs,
                              llvm::IRBuilder<>* current_irb);
  void AddInitFunc(llvm::Function* f) {
    init_funcs_.push_back(llvm::ConstantExpr::getBitCast(
        llvm::cast<llvm::Constant>(f), initfini_fn_ty_));
  }
  void AddFiniFunc(llvm::Function* f) {
    fini_funcs_.push_back(llvm::ConstantExpr::getBitCast(
        llvm::cast<llvm::Constant>(f), initfini_fn_ty_));
  }

  llvm::Module* module_ = nullptr;
  llvm::LLVMContext& ctx_;
  const wasm::Module* current_wasm_module_ = nullptr;
  // NULL-terminated list of module constructor and assert functions.
  // TODO: the ini list mechanism currently doesn't handle linking multiple
  // object files together.
  llvm::Type* initfini_fn_ty_;
  std::vector<llvm::Constant*> init_funcs_;
  std::vector<llvm::Constant*> fini_funcs_;

  std::unordered_map<const wasm::Callable*, llvm::Function*> functions_;
  llvm::Function* current_func_ = nullptr;
  std::vector<llvm::Value*> current_locals_;
  std::unordered_map<const wasm::Segment*, llvm::GlobalVariable*>
      segment_initializers_;
  llvm::IRBuilder<> irb_;
};
