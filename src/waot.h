#include "ast_visitor.h"
#include "wasm.h"
#include "wasm_ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <memory>
#include <unordered_map>

namespace llvm {
class BasicBlock;
class Function;
}

class WAOTVisitor
    : public wasm::AstVisitor<std::unique_ptr<llvm::Module>, llvm::Value*> {
 public:
  WAOTVisitor() : ctx_(llvm::getGlobalContext()) {}

 protected:
  std::unique_ptr<llvm::Module> VisitModule(const wasm::Module& mod) override;
  void VisitImport(const wasm::Import& imp) override;
  void VisitExport(const wasm::Export& exp) override;
  void VisitFunction(const wasm::Function& func) override;
  void VisitSegment(const wasm::Segment& seg) override;

  llvm::Value* VisitNop() override;
  llvm::Value* VisitBlock(const wasm::Expression::ExprVector& exprs) override;
  llvm::Value* VisitCall(bool is_import,
                         const wasm::Callable& callee,
                         int callee_index,
                         const wasm::Expression::ExprVector& args) override;
  llvm::Value* VisitReturn(const wasm::Expression::ExprVector& value) override;
  llvm::Value* VisitConst(const wasm::Literal& l) override;

 private:
  llvm::Function* GetFunction(const wasm::Callable& func,
                              llvm::Function::LinkageTypes linkage);
  llvm::LLVMContext& ctx_;
  std::unique_ptr<llvm::Module> module_;

  std::unordered_map<const wasm::Callable*, llvm::Function*> functions_;
  llvm::Function* current_func_;
  llvm::BasicBlock* current_bb_;
};
