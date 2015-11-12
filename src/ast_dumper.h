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
  void VisitMemory(Expression* expr,
                   MemoryOperator memop,
                   MemType mem_type,
                   uint32_t mem_alignment,
                   uint64_t mem_offset,
                   bool is_signed,
                   Expression* address,
                   Expression* store_val) override;
  void VisitConst(Expression* expr, Literal* l) override;
  void VisitUnop(Expression* expr,
                 UnaryOperator unop,
                 Expression* operand) override;
  void VisitBinop(Expression* epxr,
                  BinaryOperator binop,
                  Expression* lhs,
                  Expression* rhs) override;
  void VisitCompare(Expression* expr,
                    Type compare_type,
                    CompareOperator relop,
                    Expression* lhs,
                    Expression* rhs) override;
  void VisitConversion(Expression* expr,
                       ConversionOperator cvt,
                       Expression* operand) override;

  void VisitInvoke(TestScriptExpr* expr,
                   Export* callee,
                   UniquePtrVector<Expression>* args) override;
  void VisitAssertReturn(TestScriptExpr* expr,
                         TestScriptExpr* invoke_arg,
                         UniquePtrVector<Expression>* expected) override;
  void VisitAssertReturnNaN(TestScriptExpr* expr,
                            TestScriptExpr* invoke_arg) override;
  void VisitAssertTrap(TestScriptExpr* expr,
                       TestScriptExpr* invoke_arg) override;

 private:
  bool dump_types_;
  void PrintType(const Expression& expr);
  friend void DumpExpr(Expression* expr, bool dump_types);
};

void DumpExpr(Expression* expr, bool dump_types);
const char* ConversionOpName(ConversionOperator cvt);
}
#endif  // AST_DUMPER_H
