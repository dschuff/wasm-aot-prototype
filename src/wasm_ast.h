#ifndef WASM_AST_H
#define WASM_AST_H

#include "wasm.h"

#include <memory>
#include <string>
#include <vector>

namespace wasm {

class Callable;
class Module;
template <typename T>
using UniquePtrVector = std::vector<std::unique_ptr<T>>;

class Literal {
 public:
  WasmType type = WASM_TYPE_VOID;
  union {
    uint32_t i32;
    uint64_t i64;
    float f32;
    double f64;
  } value;
};

class Variable {
 public:
  Variable(WasmType t) : type(t) {}
  WasmType type = WASM_TYPE_VOID;
  int index;
  std::string local_name;  // Empty if none bound
};

class Expression {
 public:
  Expression(WasmOpcode op) : opcode(op) {}
  // Common
  WasmOpcode opcode = WASM_OPCODE_NOP;
  WasmType expr_type = WASM_TYPE_VOID;
  // Const
  Literal literal = {};
  // Call, CallImport
  int callee_index = 0;
  bool is_import = false;
  Callable* callee;
  // get_local, set_local variable
  Variable* local_var;
  // Common (block, call args, return/set_local vals)
  UniquePtrVector<Expression> exprs;
};

class Callable {
 public:
  Callable(WasmType t) : result_type(t) {}
  WasmType result_type = WASM_TYPE_VOID;
  std::string local_name;  // Empty if none bound
  UniquePtrVector<Variable> args;
};

class Function : public Callable {
 public:
  Function(WasmType t, int idx) : Callable(t), index_in_module(idx) {}
  UniquePtrVector<Variable> locals;
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
  Import(WasmType t, const std::string& m, const std::string& f)
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
  typedef enum { kAssertInvalid, kInvoke, kAssertEq, kAssertTrap } Opcode;
  TestScriptExpr(Module* mod, Opcode op) : module(mod), opcode(op) {}

  Module* module;
  Opcode opcode;
  Export* callee;                          // Invoke
  std::unique_ptr<TestScriptExpr> invoke;  // AssertEq
  UniquePtrVector<Expression> exprs;       // Invoke args, AssertEq expectation
};

}  // namespace wasm
#endif  // WASM_AST_H
