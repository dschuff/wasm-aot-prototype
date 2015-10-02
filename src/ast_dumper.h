#ifndef AST_DUMPER_H
#define AST_DUMPER_H

#include "wasm_ast.h"
#include "ast_visitor.h"

namespace wasm {

class AstDumper : public AstVisitor<void, void> {
 public:
  AstDumper(bool dump_types) : dump_types_(dump_types) {}

protected:
  void VisitModule(const Module& mod) override;
  void VisitImport(const Import& imp) override;
  void VisitExport(const Export& exp) override;
  void VisitFunction(const Function& func) override;
  void VisitSegment(const Segment& seg) override;

  void VisitExpression(Expression* expr) override {
    PrintType(*expr);
    AstVisitor::VisitExpression(expr);
  }
  void VisitNop() override;
  void VisitBlock(UniquePtrVector<Expression>* exprs) override;
  void VisitIf(Expression* condition,
               Expression* then,
               Expression* els) override;
  void VisitCall(bool is_import,
                 Callable* callee,
                 int callee_index,
                 UniquePtrVector<Expression>* args) override;
  void VisitReturn(UniquePtrVector<Expression>* value) override;
  void VisitGetLocal(Variable* var) override;
  void VisitSetLocal(Variable* var, Expression* value) override;
  void VisitConst(Literal* l) override;

  void VisitInvoke(Export* callee, UniquePtrVector<Expression>* args) override;
  void VisitAssertEq(TestScriptExpr* invoke_arg, Expression* expected) override;

 private:
  bool dump_types_;
  void PrintType(const Expression& expr);
  friend void DumpExpr(Expression* expr, bool dump_types);
};

void DumpExpr(Expression* expr, bool dump_types);
}
#endif // AST_DUMPER_H
