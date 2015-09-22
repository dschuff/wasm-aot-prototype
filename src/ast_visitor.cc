#include "ast_visitor.h"

#include "wasm_ast.h"

#include <memory>
#include <cassert>


namespace wasm {

void AstVisitor::Visit(const Module& mod) {
  VisitModule(mod);
}


void AstVisitor::VisitExpression(const Expression& expr) {
  switch (expr.opcode) {
    case WASM_OP_NOP:
      return VisitNop();
    case WASM_OP_BLOCK:
      return VisitBlock(expr.exprs);
    case WASM_OP_CALL:
    case WASM_OP_CALL_IMPORT:
      return VisitCall(expr.opcode, *expr.callee, expr.callee_index, expr.exprs);
    case WASM_OP_RETURN:
      assert(expr.exprs.size() <= 1);
      // TODO: if multiple returns are really gone, do something better
      return VisitReturn(expr.exprs);
    case WASM_OP_CONST:
      return VisitConst(expr.literal);
    default:
      assert(false);
  }
}

} // namespace wasm
