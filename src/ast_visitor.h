/*
 * Copyright 2016 WebAssembly Community Group participants
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
      case Expression::kIfElse:
        return VisitIf(expr,
                       expr->exprs[0].get(),
                       expr->exprs[1].get(),
                       expr->exprs.size() > 2 ? expr->exprs[2].get() : nullptr);
      case Expression::kCallDirect: {
        CallExpression* ce = static_cast<CallExpression*>(expr);
        return VisitCall(
            ce, ce->is_import, ce->callee, ce->callee_index, &ce->exprs);
      }
      case Expression::kReturn:
        // We don't support multiple returns anymore but it could still be void
        assert(expr->exprs.size() <= 1);
        return VisitReturn(expr, &expr->exprs);
      case Expression::kGetLocal: {
        LocalExpression* le = static_cast<LocalExpression*>(expr);
        return VisitGetLocal(le, le->local_var);
      }
      case Expression::kSetLocal: {
        LocalExpression* le = static_cast<LocalExpression*>(expr);
        return VisitSetLocal(le, le->local_var, le->exprs.front().get());
      }
      case Expression::kConst:
        return VisitConst(expr,
                          &static_cast<ConstantExpression*>(expr)->literal);
      case Expression::kUnary: {
        UnaryExpression* ue = static_cast<UnaryExpression*>(expr);
        return VisitUnop(ue, ue->unop, ue->exprs.front().get());
      }
      case Expression::kBinary: {
        BinaryExpression* be = static_cast<BinaryExpression*>(expr);
        return VisitBinop(
            be, be->binop, be->exprs[0].get(), be->exprs[1].get());
      }
      case Expression::kCompare: {
        CompareExpression* ce = static_cast<CompareExpression*>(expr);
        return VisitCompare(ce,
                            ce->compare_type,
                            ce->relop,
                            ce->exprs[0].get(),
                            ce->exprs[1].get());
      }
      case Expression::kConvert: {
        ConversionExpression* ce = static_cast<ConversionExpression*>(expr);
        return VisitConversion(ce, ce->cvt, ce->exprs.front().get());
      }
      case Expression::kMemory: {
        MemoryExpression* me = static_cast<MemoryExpression*>(expr);
        return VisitMemory(me,
                           me->memop,
                           me->mem_type,
                           me->alignment,
                           me->offset,
                           me->is_signed,
                           me->exprs[0].get(),
                           me->memop == kStore ? me->exprs[1].get() : nullptr);
      }
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

  virtual ExprVal VisitCall(CallExpression* expr,
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
  virtual ExprVal VisitGetLocal(LocalExpression* expr, Variable* var) {
    return ExprVal();
  }
  virtual ExprVal VisitSetLocal(LocalExpression* expr,
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
  virtual ExprVal VisitConversion(ConversionExpression* expr,
                                  ConversionOperator cvt,
                                  Expression* operand) {
    VisitExpression(operand);
    return ExprVal();
  }
  virtual ExprVal VisitMemory(Expression* expr,
                              MemoryOperator memop,
                              MemType mem_type,
                              uint32_t mem_alignment,
                              uint64_t mem_offset,
                              bool is_signed,
                              Expression* address,
                              Expression* store_val) {
    VisitExpression(address);
    if (store_val)
      VisitExpression(store_val);
    return ExprVal();
  }

  ExprVal VisitTestScriptExpr(TestScriptExpr* expr) {
    switch (expr->opcode) {
      case TestScriptExpr::kInvoke:
        return VisitInvoke(expr, expr->callee, &expr->exprs);
      case TestScriptExpr::kAssertReturn:
        return VisitAssertReturn(expr, expr->invoke.get(), &expr->exprs);
      case TestScriptExpr::kAssertReturnNaN:
        return VisitAssertReturnNaN(expr, expr->invoke.get());
      case TestScriptExpr::kAssertTrap:
        return VisitAssertTrap(expr, expr->invoke.get(), expr->trap_text);
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
                                    UniquePtrVector<Expression>* expected) {
    Visit(arg);
    assert(expected->size() <= 1);
    if (expected->size())
      VisitExpression(expected->front().get());
    return ExprVal();
  }
  virtual ExprVal VisitAssertReturnNaN(TestScriptExpr* expr,
                                       TestScriptExpr* arg) {
    Visit(arg);
    return ExprVal();
  }
  virtual ExprVal VisitAssertTrap(TestScriptExpr* expr,
                                  TestScriptExpr* arg,
                                  const std::string& text) {
    Visit(arg);
    return ExprVal();
  }
};
}

#endif  // AST_VISITOR_H
