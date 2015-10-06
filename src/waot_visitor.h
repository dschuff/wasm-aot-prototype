#include "ast_visitor.h"
#include "wasm.h"
#include "wasm_ast.h"

#include "llvm/IR/LLVMContext.h"
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
      : module_(llvm_module), ctx_(llvm_module->getContext()) {}

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

  llvm::Value* VisitInvoke(wasm::TestScriptExpr* expr,
                           wasm::Export* callee,
                           wasm::UniquePtrVector<wasm::Expression>*) override;
  llvm::Value* VisitAssertEq(wasm::TestScriptExpr* expr,
                             wasm::TestScriptExpr* arg,
                             wasm::Expression* expected) override;

 private:
  llvm::Function* GetFunction(const wasm::Callable& func,
                              llvm::Function::LinkageTypes linkage);
  llvm::Type* getLLVMType(wasm::Type ty);
  llvm::Constant* getAssertFailFunc(wasm::Type ty);
  llvm::Module* module_ = nullptr;
  llvm::LLVMContext& ctx_;

  std::unordered_map<const wasm::Callable*, llvm::Function*> functions_;
  llvm::Function* current_func_ = nullptr;
  std::vector<llvm::Value*> current_locals_;
  llvm::BasicBlock* current_bb_ = nullptr;
  int current_assert_eq_ = 0;
};
