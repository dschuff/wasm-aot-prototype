#include "ast_visitor.h"
#include "wasm.h"
#include "wasm_ast.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <memory>

namespace llvm {
class Module;
}

class AstValue {
};

class WAOTVisitor : public wasm::AstVisitor<std::unique_ptr<llvm::Module>, AstValue> {
 public:
  WAOTVisitor() : ctx_(llvm::getGlobalContext()) {}
protected:
  std::unique_ptr<llvm::Module> VisitModule(const wasm::Module& mod) override;
  void VisitImport(const wasm::Import& imp) override;
  void VisitFunction(const wasm::Function& func) override;
  void VisitSegment(const wasm::Segment& seg) override;

  AstValue VisitNop() override;
  AstValue VisitBlock(const wasm::Expression::ExprVector& exprs) override;
  AstValue VisitCall(WasmOpType opcode,
                     const wasm::Callable& callee,
                     int callee_index,
                     const wasm::Expression::ExprVector& args) override;
  AstValue VisitReturn(const wasm::Expression::ExprVector& value) override;
  AstValue VisitConst(const wasm::Literal& l) override;
 private:
  llvm::LLVMContext &ctx_;
  std::unique_ptr<llvm::Module> module_;
};
