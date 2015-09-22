#include "wasm.h"

class AstValue {
};

class WAOTVisitor : public wasm::AstVisitor<AstValue> {
protected:
  void VisitModule(const wasm::Module& mod) override;
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
};
