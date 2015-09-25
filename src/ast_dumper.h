#ifndef AST_DUMPER_H
#define AST_DUMPER_H

#include "wasm_ast.h"
#include "ast_visitor.h"

namespace wasm {

class AstDumper : public AstVisitor<void, void> {
protected:
  void VisitModule(const Module& mod) override;
  void VisitImport(const Import& imp) override;
  void VisitExport(const Export& exp) override;
  void VisitFunction(const Function& func) override;
  void VisitSegment(const Segment& seg) override;

  void VisitNop() override;
  void VisitBlock(const UniquePtrVector<Expression>& exprs) override;
  void VisitCall(bool is_import,
                 const Callable& callee,
                 int callee_index,
                 const UniquePtrVector<Expression>& args) override;
  void VisitReturn(const UniquePtrVector<Expression>& value) override;
  void VisitConst(const Literal& l) override;

  void VisitInvoke(const Export& callee,
                   const UniquePtrVector<Expression>& args) override;
  void VisitAssertEq(const TestScriptExpr& invoke_arg,
                     const Expression& expected) override;
};
}

#endif // AST_DUMPER_H
