#ifndef AST_VISITOR_H
#define AST_VISITOR_H

#include "wasm_ast.h"

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

class AstVisitor {
public:
  void Visit(const Module& mod);
 protected:
  virtual void VisitModule(const Module& mod) = 0;
  virtual void VisitImport(const Import& imp) = 0;
  // TODO: define VisitExport for symmetry?
  virtual void VisitFunction(const Function& func) = 0;
  virtual void VisitSegment(const Segment& seg) = 0;

  void VisitExpression(const Expression& expr);
  virtual void VisitNop() = 0;
  virtual void VisitBlock(const Expression::ExprVector& exprs) = 0;
  virtual void VisitCall(WasmOpType opcode,
                         const Callable& callee,
                         int callee_index,
                         const Expression::ExprVector& args) = 0;
  virtual void VisitReturn(const Expression::ExprVector& value) = 0;
  virtual void VisitConst(const Literal& l) = 0;
};
}

#endif // AST_VISITOR_H
