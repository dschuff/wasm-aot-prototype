#include "ast_visitor.h"

#include "wasm_ast.h"

#include <memory>
#include <cassert>


namespace wasm {

// TODO: Move this
template<typename ExprVal>
void AstVisitor<ExprVal>::Visit(const Module& mod) {
  VisitModule(mod);
}


} // namespace wasm
