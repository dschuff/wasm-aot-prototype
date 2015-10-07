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

static_assert(WASM_NUM_TYPES == 5, "wasm::Type enum needs to be adjusted");
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
    kAny = WASM_NUM_TYPES,  // TODO: Is this useful? could just void
    kUnknown,
  } Type_;
  Type(Type_ t) : value_(t) {}
  Type(WasmType t) : value_(static_cast<Type_>(t)) {
    assert(t < WASM_NUM_TYPES && "Bad Type initializer");
  }
  operator Type_() const { return value_; }
  explicit operator WasmType() const { return static_cast<WasmType>(value_); }

 private:
  Type_ value_ = kUnknown;
};

// TODO: do we need a hierarchy of operators like the spec?
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
  kMax
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
  Expression(WasmOpcode op) : opcode(op) {}
  // Common
  WasmOpcode opcode = WASM_OPCODE_NOP;
  Type expr_type = Type::kUnknown;
  Type expected_type = Type::kUnknown;
  // Const
  Literal literal = {};
  // Call, CallImport
  int callee_index = 0;
  bool is_import = false;
  Callable* callee;
  // get_local, set_local variable
  Variable* local_var;
  // Binops
  BinaryOperator binop;
  // Compare
  Type compare_type = Type::kUnknown;
  CompareOperator relop;
  // Common (block, call args, return/set_local vals, compare operands)
  UniquePtrVector<Expression> exprs;
};

class Callable {
 public:
  Callable(Type t) : result_type(t) {}
  Type result_type = Type::kVoid;
  std::string local_name;  // Empty if none bound
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
  TestScriptExpr(Module* mod, Opcode op)
      : module(mod), opcode(op), type(Type::kUnknown) {}

  Module* module;
  Opcode opcode;
  Export* callee;                          // Invoke
  Type type;                               // AssertReturn
  std::unique_ptr<TestScriptExpr> invoke;  // AssertReturn
  UniquePtrVector<Expression> exprs;       // Invoke args, AR expectation
};

}  // namespace wasm
#endif  // WASM_AST_H
