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

#ifndef WASM_AST_H
#define WASM_AST_H

#include "ast.h"

#include <cassert>
#include <memory>
#include <string>
#include <vector>

namespace wasm {

class Module;
template <typename T>
using UniquePtrVector = std::vector<std::unique_ptr<T>>;

static_assert(WASM_TYPE_ANY == 0, "wasm::Type enum needs to be adjusted");
// Type is basically just an enum, but implicitly convertible from WasmType.
// It also includes an "unknown" state used during construction/type fixup.
class Type {
 public:
  typedef enum Type_ {
    kVoid = WASM_TYPE_VOID,
    kI32 = WASM_TYPE_I32,
    kI64 = WASM_TYPE_I64,
    kF32 = WASM_TYPE_F32,
    kF64 = WASM_TYPE_F64,
    kAny = WASM_TYPE_ANY,
    kUnknown,
  } Type_;
  Type(Type_ t) : value_(t) {}
  Type(WasmType t) : value_(static_cast<Type_>(t)) {
    assert(t <= WASM_TYPE_ANY && "Bad Type initializer");
  }
  operator Type_() const { return value_; }
  explicit operator WasmType() const { return static_cast<WasmType>(value_); }
  bool IsInt() { return value_ == kI32 || value_ == kI64; }
  bool IsFloat() { return value_ == kF32 || value_ == kF64; }

 private:
  Type_ value_ = kUnknown;
};

class MemType {
 public:
  typedef enum Type_ {
    // TODO: remove this type distinction and just go with size instead?
    kI8,
    kI16,
    kI32,
    kI64,
    kF32,
    kF64,
    kUnknown,
  } Type_;
  MemType(Type_ t) : value_(t) {}
  operator Type_() const { return value_; }
  // Compare against a value type.
  bool operator==(const Type& other) const {
    return (value_ == kI32 && other == Type::kI32) ||
           (value_ == kI64 && other == Type::kI64) ||
           (value_ == kF32 && other == Type::kF32) ||
           (value_ == kF64 && other == Type::kF64);
  }
  bool IsFloatTy() const { return value_ == kF32 || value_ == kF64; }
  unsigned GetSizeInBits() const {
    static_assert(kI8 == 0, "wrong value for kI8");
    static_assert(kI16 == 1, "wrong value for kI16");
    static_assert(kI32 == 2, "wrong value for kI32");
    static_assert(kI64 == 3, "wrong value for kI64");
    static_assert(kF32 == 4, "wrong value for kF32");
    static_assert(kF64 == 5, "wrong value for kF64");
    return value_ <= kI64 ? 8 << value_ : 32 << (value_ - kF32);
  };

 private:
  Type_ value_ = kUnknown;
};

// TODO: do we need a hierarchy of operators like the spec?
enum UnaryOperator {
  // Int
  kClz,
  kCtz,
  kPopcnt,
  // FP
  kNeg,
  kAbs,
  kCeil,
  kFloor,
  kTrunc,
  kNearest,
  kSqrt,
};

enum BinaryOperator {
  kAdd,
  kSub,
  kMul,
  // Int
  kDivS,
  kDivU,
  kRemS,
  kRemU,
  kAnd,
  kOr,
  kXor,
  kShl,
  kShrU,
  kShrS,
  // FP
  kDiv,
  kCopySign,
  kMin,
  kMax,
};

enum CompareOperator {
  kEq,
  kNE,
  // Int
  kLtS,
  kLtU,
  kLeS,
  kLeU,
  kGtS,
  kGtU,
  kGeS,
  kGeU,
  // FP
  kLt,
  kLe,
  kGt,
  kGe,
};

enum ConversionOperator {
  // Int
  kExtendSInt32,
  kExtendUInt32,
  kWrapInt64,
  kTruncSFloat32,
  kTruncUFloat32,
  kTruncSFloat64,
  kTruncUFloat64,
  kReinterpretFloat,
  // FP
  kConvertSInt32,
  kConvertUInt32,
  kConvertSInt64,
  kConvertUInt64,
  kPromoteFloat32,
  kDemoteFloat64,
  kReinterpretInt,
};

enum MemoryOperator {
  kLoad,
  kStore,
};

class Literal {
 public:
  Type type = Type::kUnknown;
  union {
    uint32_t i32;
    uint64_t i64;
    // TODO: Make these bag-of-bits instead of C++ types
    float f32;
    double f64;
  } value;
};

class Variable {
 public:
  Variable(Type t) : type(t) {}
  Type type = Type::kUnknown;
  int index;
  std::string local_name;  // Empty if none bound
};

class Callable {
 public:
  Callable(Type t, const WasmStringSlice& name)
      : result_type(t), local_name(name.start, name.length) {}
  Type result_type = Type::kVoid;
  std::string local_name;            // Empty if none bound
  UniquePtrVector<Variable> locals;  // Includes the args at the front
  std::vector<Variable*> args;       // Convenience pointers to the args
};

class Expression {
 public:
  // ExpressionKind is used instead of RTTI. Only AstVisitor::VisitExpression
  // actually
  // needs to care about this field currently.
  enum ExpressionKind {
    kNop,
    kBlock,
    kIf,
    kIfElse,
    kCallDirect,
    kDrop,
    kReturn,
    kGetLocal,
    kSetLocal,
    kConst,
    kUnary,
    kBinary,
    kCompare,
    kConvert,
    kMemory,
  };
  Expression(ExpressionKind k, Type ty, Type expected_ty)
      : kind(k), expr_type(ty), expected_type(expected_ty) {}
  ExpressionKind kind;
  Type expr_type = Type::kUnknown;
  Type expected_type = Type::kUnknown;
  // Operands
  UniquePtrVector<Expression> exprs;
};

class ConstantExpression final : public Expression {
 public:
  ConstantExpression(Type ty, Type expected_ty)
      : Expression(kConst, ty, expected_ty) {
    literal.type = ty;
  };
  Literal literal;
};

class CallExpression final : public Expression {
 public:
  CallExpression(int idx, bool imp, Callable* c, Type expected_ty)
      : Expression(kCallDirect, c->result_type, expected_ty),
        callee_index(idx),
        is_import(imp),
        callee(c) {};
  int callee_index;
  bool is_import;
  Callable* callee;
};

class LocalExpression final : public Expression {
 public:
  // TODO: After updating to AST parser, consider standardizing on factories
  // everywhere, or constructors everywhere.
  static LocalExpression* GetGetLocal(Variable* v, Type expected_ty) {
    return new LocalExpression(kGetLocal, v, expected_ty);
  }
  static LocalExpression* GetSetLocal(Variable* v, Type expected_ty) {
    return new LocalExpression(kSetLocal, v, expected_ty);
  }
  Variable* local_var;

 private:
  LocalExpression(ExpressionKind k, Variable* v, Type expected_ty)
      : Expression(k, v->type, expected_ty), local_var(v) {};
};

class MemoryExpression final : public Expression {
 public:
  MemoryExpression(MemoryOperator op,
                   Type ty,
                   Type expected_ty,
                   MemType mem_ty,
                   uint32_t align,
                   uint64_t off,
                   bool sign)
      : Expression(kMemory, ty, expected_ty),
        memop(op),
        mem_type(mem_ty),
        alignment(align),
        offset(off),
        is_signed(sign) {};
  MemoryOperator memop;
  MemType mem_type = MemType::kUnknown;
  uint32_t alignment;
  uint64_t offset;
  bool is_signed;
};

class UnaryExpression final : public Expression {
 public:
  UnaryExpression(UnaryOperator op, Type ty, Type expected_ty)
      : Expression(kUnary, ty, expected_ty), unop(op) {};
  UnaryOperator unop;
};

class BinaryExpression final : public Expression {
 public:
  BinaryExpression(BinaryOperator op, Type ty, Type expected_ty)
      : Expression(kBinary, ty, expected_ty), binop(op) {};
  BinaryOperator binop;
};

class CompareExpression final : public Expression {
 public:
  CompareExpression(Type ct, CompareOperator op, Type expected_ty)
      : Expression(kCompare, Type::kI32, expected_ty),
        compare_type(ct),
        relop(op) {};
  Type compare_type = Type::kUnknown;
  CompareOperator relop;
};

class ConversionExpression final : public Expression {
 public:
  ConversionExpression(ConversionOperator op,
                       Type op_ty,
                       Type result_ty,
                       Type expected_ty)
      : Expression(kConvert, result_ty, expected_ty),
        cvt(op),
        operand_type(op_ty) {};
  ConversionOperator cvt;
  //   Technically redundant with cvt, but handy to have.
  Type operand_type = Type::kUnknown;
};

class Function : public Callable {
 public:
  Function(Type t, const WasmStringSlice& name, int idx)
      : Callable(t, name), index_in_module(idx) {}
  UniquePtrVector<Expression> body;
  int index_in_module = 0;
};

class Export {
 public:
  Export(Function* f, const WasmStringSlice& n, Module* m)
      : function(f), name(n.start, n.length), module(m) {}
  Function* function;
  std::string name;
  Module* module;
};

class Import : public Callable {
 public:
  Import(Type t,
         const WasmStringSlice& name,
         const WasmStringSlice& m,
         const WasmStringSlice& f)
      : Callable(t, name),
        module_name(m.start, m.length),
        func_name(f.start, f.length) {}
  std::string module_name;
  std::string func_name;
};

class Segment {
 public:
  Segment(size_t sz, size_t addr) : size(sz), address(addr) {}
  std::string as_string() const;
  size_t size = 0;
  size_t address = 0;
  std::vector<char> initial_data;
};

class Module {
 public:
  static constexpr const uint32_t kPageSize =  16 * 1024;
  UniquePtrVector<Segment> segments;
  UniquePtrVector<Function> functions;
  UniquePtrVector<Export> exports;
  UniquePtrVector<Import> imports;
  uint32_t initial_memory_size = 0;
  uint32_t max_memory_size = 0;
  std::string name;
};

class SourceLocation {
 public:
  SourceLocation(const WasmLocation& loc)
      : filename(loc.filename), line(loc.line), col(loc.first_column) {}
  // For now there's only ever one file per run, but it's simpler to keep the
  // filename here.  If there gets to be SourceLocs for every expr, we probably
  // want to dedup the filename info.
  std::string filename = "";
  int line = 0;
  int col = 0;
  // TODO: add last line/col
};

// For spec repo test scripts. The spec test script operations are basically
// expressions but have no wasm opcodes and they exist outside modules, so at
// the top level they have their own classes, which contain Expressions.  Each
// script expression refers to the module that immediately preceeds it in the
// file (the parser checks this and sets up the mapping when creating the AST).
// TODO: make this a hierarchy like exprs.
class TestScriptExpr {
 public:
  typedef enum {
    kAssertInvalid,
    kInvoke,
    kAssertReturn,
    kAssertReturnNaN,
    kAssertTrap
  } Opcode;
  TestScriptExpr(Module* mod, Opcode op, const WasmLocation loc)
      : module(mod), opcode(op), source_loc(loc), type(Type::kUnknown) {}

  Module* module;
  Opcode opcode;
  SourceLocation source_loc;
  Export* callee;                          // Invoke
  Type type;                               // AssertReturn, Invoke
  std::unique_ptr<TestScriptExpr> invoke;  // AssertReturn, AssertTrap
  std::string trap_text;                   // AssertTrap
  UniquePtrVector<Expression> exprs;       // Invoke args, AR expectation
};

}  // namespace wasm
#endif  // WASM_AST_H
