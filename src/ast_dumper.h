#ifndef AST_DUMPER_H
#define AST_DUMPER_H

#include "wasm_ast.h"
#include "ast_visitor.h"

namespace wasm {

class AstDumper : public AstVisitor<void> {
protected:
  void VisitModule(const Module& mod) override;
  void VisitImport(const Import& imp) override;
  void VisitFunction(const Function& func) override;
  void VisitSegment(const Segment& seg) override;

  void VisitNop() override;
  void VisitBlock(const Expression::ExprVector& exprs) override;
  void VisitCall(WasmOpType opcode,
                 const Callable& callee,
                 int callee_index,
                 const Expression::ExprVector& args) override;
  void VisitReturn(const Expression::ExprVector& value) override;
  void VisitConst(const Literal& l) override;
};
}

#endif // AST_DUMPER_H
