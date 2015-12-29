#ifndef WASM_AST_H
#define WASM_AST_H

#include "wasm.h"

#include <cassert>
#include <memory>
#include <string>
#include <vector>

namespace wasm {

class Callable;
class Module;
template <typename T>
using UniquePtrVector = std::vector<std::unique_ptr<T>>;

static_assert(WASM_TYPE_ALL == 15, "wasm::Type enum needs to be adjusted");
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
    kAny = WASM_TYPE_ALL,
    kUnknown,
  } Type_;
  Type(Type_ t) : value_(t) {}
  Type(WasmType t) : value_(static_cast<Type_>(t)) {
    assert(t <= WASM_TYPE_ALL && "Bad Type initializer");
  }
  operator Type_() const { return value_; }
  explicit operator WasmType() const { return static_cast<WasmType>(value_); }

 private:
  Type_ value_ = kUnknown;
};

class MemType {
 public:
  typedef enum Type_ {
    kI8 = WASM_MEM_TYPE_I8,
    kI16 = WASM_MEM_TYPE_I16,
    kI32 = WASM_MEM_TYPE_I32,
    kI64 = WASM_MEM_TYPE_I64,
    kF32 = WASM_MEM_TYPE_F32,
    kF64 = WASM_MEM_TYPE_F64,
    kUnknown,
  } Type_;
  MemType(Type_ t) : value_(t) {}
  MemType(WasmMemType t) : value_(static_cast<Type_>(t)) {
    assert(t < WASM_NUM_MEM_TYPES && "Bad Type initializer");
  }
  operator Type_() const { return value_; }
  explicit operator WasmMemType() const {
    return static_cast<WasmMemType>(value_);
  }
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

class Expression {
 public:
  // ExpressionKind is used instead of RTTI. Only AstVisitor::VisitExpression actually
  // needs to care about this field currently.
  enum ExpressionKind {
    kNop,
    kBlock,
    kIf,
    kCallDirect,
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
  Expression(ExpressionKind k) : kind(k) {}
  ExpressionKind kind;
  Type expr_type = Type::kUnknown;
  Type expected_type = Type::kUnknown;
  // Operands
  UniquePtrVector<Expression> exprs;
};

class ConstantExpression final : public Expression {
 public:
 ConstantExpression(const Type ty) : Expression(kConst) { literal.type = ty; };
  Literal literal;
};

class CallExpression final: public Expression {
 public:
 CallExpression(int idx, bool imp, Callable* c) :
  Expression(kCallDirect), callee_index(idx), is_import(imp), callee(c) {};
 int callee_index;
 bool is_import;
 Callable* callee;
};

class LocalExpression final : public Expression {
 public:
  // TODO: After updating to AST parser, consider standardizing on factories
  // everywhere, or constructors everywhere.
  static LocalExpression* GetGetLocal(Variable* v) {
    return new LocalExpression(kGetLocal, v);
  }
  static LocalExpression* GetSetLocal(Variable* v) {
    return new LocalExpression(kSetLocal, v);
  }
  Variable* local_var;
 private:
 LocalExpression(ExpressionKind k, Variable *v) : Expression(k), local_var(v) {};
};

class MemoryExpression final: public Expression {
 public:
  MemoryExpression(MemoryOperator op, MemType t, uint32_t align,
		   uint64_t off, bool sign) :
  Expression(kMemory), memop(op), mem_type(t), alignment(align), offset(off), is_signed(sign) {};
  MemoryOperator memop;
  MemType mem_type = MemType::kUnknown;
  uint32_t alignment;
  uint64_t offset;
  bool is_signed;
};

class UnaryExpression final : public Expression {
 public:
 UnaryExpression(UnaryOperator op) : Expression(kUnary), unop(op) {};
 UnaryOperator unop;
};

class BinaryExpression final: public Expression {
 public:
 BinaryExpression(BinaryOperator op) : Expression(kBinary), binop(op) {};
 BinaryOperator binop;
};

class CompareExpression final: public Expression {
 public:
 CompareExpression(Type ct, CompareOperator op) :
  Expression(kCompare), compare_type(ct), relop(op) {};
 Type compare_type = Type::kUnknown;
 CompareOperator relop;
};

class ConversionExpression final: public Expression {
 public:
 ConversionExpression(ConversionOperator op, Type opty) :
  Expression(kConvert), cvt(op), operand_type(opty) {};
 ConversionOperator cvt;
 //   Technically redundant with cvt, but handy to have.
 Type operand_type = Type::kUnknown;
};



class Callable {
 public:
  Callable(Type t) : result_type(t) {}
  Type result_type = Type::kVoid;
  std::string local_name;            // Empty if none bound
  UniquePtrVector<Variable> locals;  // Includes the args at the front
  std::vector<Variable*> args;       // Convenience pointers to the args
};

class Function : public Callable {
 public:
  Function(Type t, int idx) : Callable(t), index_in_module(idx) {}
  UniquePtrVector<Expression> body;
  int index_in_module = 0;
};

class Export {
 public:
  Export(Function* f, const std::string& n, Module* m)
      : function(f), name(n), module(m) {}
  Function* function;
  std::string name;
  Module* module;
};

class Import : public Callable {
 public:
  Import(Type t, const std::string& m, const std::string& f)
      : Callable(t), module_name(m), func_name(f) {}
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
  SourceLocation(const WasmSourceLocation& loc)
      : filename(loc.source->filename), line(loc.line), col(loc.col) {}
  // For now there's only ever one file per run, but it's simpler to keep the
  // filename here.  If there gets to be SourceLocs for every expr, we probably
  // want to dedup the filename info.
  std::string filename = "";
  int line = 0;
  int col = 0;
};

// For spec repo test scripts. The spec test script operations are basically
// expressions but have no wasm opcodes and they exist outside modules, so at
// the top level they have their own classes, which contain Expressions.  Each
// script expression refers to the module that immediately preceeds it in the
// file (the parser checks this and sets up the mapping when creating the AST).
class TestScriptExpr {
 public:
  typedef enum {
    kAssertInvalid,
    kInvoke,
    kAssertReturn,
    kAssertReturnNaN,
    kAssertTrap
  } Opcode;
  TestScriptExpr(Module* mod, Opcode op, const WasmSourceLocation loc)
      : module(mod), opcode(op), source_loc(loc), type(Type::kUnknown) {}

  Module* module;
  Opcode opcode;
  SourceLocation source_loc;
  Export* callee;                          // Invoke
  Type type;                               // AssertReturn, Invoke
  std::unique_ptr<TestScriptExpr> invoke;  // AssertReturn, AssertTrap
  UniquePtrVector<Expression> exprs;       // Invoke args, AR expectation
};

}  // namespace wasm
#endif  // WASM_AST_H
