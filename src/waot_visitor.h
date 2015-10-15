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
}

class FunctionInfo;

class WAOTVisitor : public wasm::AstVisitor<llvm::Module*, llvm::Value*> {
 public:
  WAOTVisitor(llvm::Module* llvm_module)
      : module_(llvm_module),
        ctx_(llvm_module->getContext()),
        irb_(llvm_module->getContext()) {}

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
      wasm::Expression* expr,
      bool is_import,
      wasm::Callable* callee,
      int callee_index,
      wasm::UniquePtrVector<wasm::Expression>* args) override;
  llvm::Value* VisitReturn(
      wasm::Expression* expr,
      wasm::UniquePtrVector<wasm::Expression>* value) override;
  llvm::Value* VisitGetLocal(wasm::Expression* expr,
                             wasm::Variable* var) override;
  llvm::Value* VisitSetLocal(wasm::Expression* expr,
                             wasm::Variable* var,
                             wasm::Expression* value) override;
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
  llvm::Value* VisitConversion(wasm::Expression* expr,
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
  llvm::Module* module_ = nullptr;
  llvm::LLVMContext& ctx_;

  std::unordered_map<const wasm::Callable*, llvm::Function*> functions_;
  llvm::Function* current_func_ = nullptr;
  std::vector<llvm::Value*> current_locals_;
  llvm::IRBuilder<> irb_;
};
