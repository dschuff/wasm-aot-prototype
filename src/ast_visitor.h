#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#include "wasm_ast.h"

#include <cassert>

namespace wasm {

// Visitor-ish pattern; abstracts the data layout of Expression, but driven by
// the implementation. Visit is the entry. The implementation of VisitModule is
// expected to call Visit{Function,Segment,Import}. The VisitFunction impl calls
// VisitExpression.

// The base VisitExpression implementation calls the apropriate derived-class
// function for the expression type and passes in the relevant arguments. The
// VisitFoo implementation is expected to then call VisitExpression the
// expressions therein, but it can do whatever it needs to before and/or after
// that.

// TODO: We could do that same thing with VisitModule (e.g. pass it lists of
// functions, segments, imports, exports) to make it uniform. Realistically it
// might make more sense just to keep the extra data available to the derived
// implementations though.

template <typename ModuleVal, typename ExprVal>
class AstVisitor {
public:
 ModuleVal Visit(const Module& mod) { return VisitModule(mod); }
 ExprVal Visit(TestScriptExpr* script) { return VisitTestScriptExpr(script); }

 protected:
  virtual ModuleVal VisitModule(const Module& mod) {
    for (auto& func : mod.functions)
      VisitFunction(*func);

    if (mod.initial_memory_size) {
      for (auto& seg : mod.segments)
        VisitSegment(*seg);
    }
    for (auto& imp : mod.imports)
      VisitImport(*imp);
    for (auto& ex : mod.exports)
      VisitExport(*ex);
    return ModuleVal();
  }
  virtual void VisitImport(const Import& imp) {}
  virtual void VisitExport(const Export& exp) {}
  virtual void VisitFunction(const Function& func) {
    for (auto& expr : func.body)
      VisitExpression(expr.get());
  }
  virtual void VisitSegment(const Segment& seg) {}

  virtual ExprVal VisitExpression(Expression* expr) {
    switch (expr->kind) {
      case Expression::kNop:
        return VisitNop(expr);
      case Expression::kBlock:
        return VisitBlock(expr, &expr->exprs);
      case Expression::kIf:
        return VisitIf(expr, expr->exprs[0].get(), expr->exprs[1].get(),
                       expr->exprs.size() > 2 ? expr->exprs[2].get() : nullptr);
      case Expression::kCallDirect:
        return VisitCall(expr, expr->is_import, expr->callee,
                         expr->callee_index, &expr->exprs);
      case Expression::kReturn:
        // We don't support multiple returns anymore but it could still be void
        assert(expr->exprs.size() <= 1);
        return VisitReturn(expr, &expr->exprs);
      case Expression::kGetLocal:
        return VisitGetLocal(expr, expr->local_var);
      case Expression::kSetLocal:
        return VisitSetLocal(expr, expr->local_var, expr->exprs.front().get());
      case Expression::kConst:
        return VisitConst(expr, &expr->literal);
      case Expression::kUnary:
        return VisitUnop(expr, expr->unop, expr->exprs.front().get());
      case Expression::kBinary:
        return VisitBinop(expr, expr->binop, expr->exprs[0].get(),
                          expr->exprs[1].get());
      case Expression::kCompare:
        return VisitCompare(expr, expr->compare_type, expr->relop,
                            expr->exprs[0].get(), expr->exprs[1].get());
      default:
        assert(false);
    }
  }
  virtual ExprVal VisitNop(Expression* expr) { return ExprVal(); }
  virtual ExprVal VisitBlock(Expression* expr,
                             UniquePtrVector<Expression>* exprs) {
    for (auto& e : *exprs)
      VisitExpression(e.get());
    return ExprVal();
  }
  virtual ExprVal VisitIf(Expression* expr,
                          Expression* condition,
                          Expression* then,
                          Expression* els) {
    VisitExpression(condition);
    VisitExpression(then);
    if (els)
      VisitExpression(els);
    return ExprVal();
  }

  virtual ExprVal VisitCall(Expression* expr,
                            bool is_import,
                            Callable* callee,
                            int callee_index,
                            UniquePtrVector<Expression>* args) {
    for (auto& e : *args)
      VisitExpression(e.get());
    return ExprVal();
  }
  virtual ExprVal VisitReturn(Expression* expr,
                              UniquePtrVector<Expression>* value) {
    if (value->size())
      VisitExpression(value->front().get());
    return ExprVal();
  }
  virtual ExprVal VisitGetLocal(Expression* expr, Variable* var) {
    return ExprVal();
  }
  virtual ExprVal VisitSetLocal(Expression* expr,
                                Variable* var,
                                Expression* value) {
    VisitExpression(value);
    return ExprVal();
  }
  virtual ExprVal VisitConst(Expression* expr, Literal* l) { return ExprVal(); }
  virtual ExprVal VisitUnop(Expression* expr,
                            UnaryOperator unop,
                            Expression* operand) {
    VisitExpression(operand);
    return ExprVal();
  }
  virtual ExprVal VisitBinop(Expression* epxr,
                             BinaryOperator binop,
                             Expression* lhs,
                             Expression* rhs) {
    VisitExpression(lhs);
    VisitExpression(rhs);
    return ExprVal();
  }
  virtual ExprVal VisitCompare(Expression* expr,
                               Type compare_type,
                               CompareOperator relop,
                               Expression* lhs,
                               Expression* rhs) {
    VisitExpression(lhs);
    VisitExpression(rhs);
    return ExprVal();
  }

  ExprVal VisitTestScriptExpr(TestScriptExpr* expr) {
    switch (expr->opcode) {
      case TestScriptExpr::kInvoke:
        return VisitInvoke(expr, expr->callee, &expr->exprs);
      case TestScriptExpr::kAssertReturn:
        return VisitAssertReturn(expr, expr->invoke.get(),
                                 expr->exprs[0].get());
      case TestScriptExpr::kAssertTrap:
        return VisitAssertTrap(expr, expr->invoke.get());
      default:
        assert(false);
    }
  }
  virtual ExprVal VisitInvoke(TestScriptExpr* expr,
                              Export* callee,
                              UniquePtrVector<Expression>* args) {
    for (auto& e : *args)
      VisitExpression(e.get());
    return ExprVal();
  }
  virtual ExprVal VisitAssertReturn(TestScriptExpr* expr,
                                    TestScriptExpr* arg,
                                    Expression* expected) {
    Visit(arg);
    VisitExpression(expected);
    return ExprVal();
  }
  virtual ExprVal VisitAssertTrap(TestScriptExpr* expr, TestScriptExpr* arg) {
    Visit(arg);
    return ExprVal();
  }
};
}

#endif // AST_VISITOR_H
