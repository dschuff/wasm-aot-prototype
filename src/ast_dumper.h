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
  void VisitNop(Expression* expr) override;
  void VisitBlock(Expression* expr,
                  UniquePtrVector<Expression>* exprs) override;
  void VisitIf(Expression* expr,
               Expression* condition,
               Expression* then,
               Expression* els) override;
  void VisitCall(Expression* expr,
                 bool is_import,
                 Callable* callee,
                 int callee_index,
                 UniquePtrVector<Expression>* args) override;
  void VisitReturn(Expression* expr,
                   UniquePtrVector<Expression>* value) override;
  void VisitGetLocal(Expression* expr, Variable* var) override;
  void VisitSetLocal(Expression* expr,
                     Variable* var,
                     Expression* value) override;
  void VisitConst(Expression* expr, Literal* l) override;
  void VisitCompare(Expression* expr,
                    Type compare_type,
                    CompareOperator relop,
                    Expression* lhs,
                    Expression* rhs) override;

  void VisitInvoke(TestScriptExpr* expr,
                   Export* callee,
                   UniquePtrVector<Expression>* args) override;
  void VisitAssertReturn(TestScriptExpr* expr,
                         TestScriptExpr* invoke_arg,
                         Expression* expected) override;

 private:
  bool dump_types_;
  void PrintType(const Expression& expr);
  friend void DumpExpr(Expression* expr, bool dump_types);
};

void DumpExpr(Expression* expr, bool dump_types);
}
#endif // AST_DUMPER_H
