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
 ExprVal Visit(const TestScriptExpr& script) {
   return VisitTestScriptExpr(script);
 }

 protected:
  virtual ModuleVal VisitModule(const Module& mod) = 0;
  virtual void VisitImport(const Import& imp) = 0;
  virtual void VisitExport(const Export& exp) = 0;
  virtual void VisitFunction(const Function& func) = 0;
  virtual void VisitSegment(const Segment& seg) = 0;

  ExprVal VisitExpression(const Expression& expr) {
    switch (expr.opcode) {
      case WASM_OPCODE_NOP:
        return VisitNop();
      case WASM_OPCODE_BLOCK:
        return VisitBlock(expr.exprs);
      case WASM_OPCODE_CALL:
        return VisitCall(expr.is_import, *expr.callee, expr.callee_index, expr.exprs);
      case WASM_OPCODE_RETURN:
        assert(expr.exprs.size() <= 1);
        // TODO: if multiple returns are really gone, do something better
        return VisitReturn(expr.exprs);
      case WASM_OPCODE_I8_CONST:
      case WASM_OPCODE_I32_CONST:
      case WASM_OPCODE_I64_CONST:
      case WASM_OPCODE_F32_CONST:
      case WASM_OPCODE_F64_CONST:
        return VisitConst(expr.literal);
      default:
        assert(false);
    }
  }
  virtual ExprVal VisitNop() = 0;
  virtual ExprVal VisitBlock(const UniquePtrVector<Expression>& exprs) = 0;
  virtual ExprVal VisitCall(bool is_import,
                            const Callable& callee,
                            int callee_index,
                            const UniquePtrVector<Expression>& args) = 0;
  virtual ExprVal VisitReturn(const UniquePtrVector<Expression>& value) = 0;
  virtual ExprVal VisitConst(const Literal& l) = 0;

  ExprVal VisitTestScriptExpr(const TestScriptExpr& expr) {
    switch (expr.opcode) {
      case TestScriptExpr::kInvoke:
        return VisitInvoke(*expr.callee, expr.exprs);
      case TestScriptExpr::kAssertEq:
        return VisitAssertEq(*expr.invoke, *expr.exprs[0]);
      default:
        assert(false);
    }
  }
  virtual ExprVal VisitInvoke(const Export& callee,
                              const UniquePtrVector<Expression>&) = 0;
  virtual ExprVal VisitAssertEq(const TestScriptExpr& arg,
                                const Expression& expected) = 0;
};
}

#endif // AST_VISITOR_H
