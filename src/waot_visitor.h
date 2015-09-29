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

  llvm::Value* VisitNop() override;
  llvm::Value* VisitBlock(
      const wasm::UniquePtrVector<wasm::Expression>& exprs) override;
  llvm::Value* VisitCall(
      bool is_import,
      const wasm::Callable& callee,
      int callee_index,
      const wasm::UniquePtrVector<wasm::Expression>& args) override;
  llvm::Value* VisitReturn(
      const wasm::UniquePtrVector<wasm::Expression>& value) override;
  llvm::Value* VisitGetLocal(const wasm::Variable& var) override;
  llvm::Value* VisitSetLocal(const wasm::Variable& var,
                             const wasm::Expression& value) override;
  llvm::Value* VisitConst(const wasm::Literal& l) override;

  llvm::Value* VisitInvoke(
      const wasm::Export& callee,
      const wasm::UniquePtrVector<wasm::Expression>&) override;
  llvm::Value* VisitAssertEq(const wasm::TestScriptExpr& arg,
                             const wasm::Expression& expected) override;

 private:
  llvm::Function* GetFunction(const wasm::Callable& func,
                              llvm::Function::LinkageTypes linkage);
  llvm::Module* module_ = nullptr;
  llvm::LLVMContext& ctx_;

  std::unordered_map<const wasm::Callable*, llvm::Function*> functions_;

  llvm::Function* current_func_ = nullptr;
  // std::vector<llvm::Value*> current_args_;
  std::vector<llvm::Value*> current_locals_;
  llvm::BasicBlock* current_bb_ = nullptr;
};
