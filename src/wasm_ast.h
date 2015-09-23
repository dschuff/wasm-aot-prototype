#ifndef WASM_AST_H
#define WASM_AST_H

#include "wasm.h"

#include <memory>
#include <string>
#include <vector>

namespace wasm {

class Callable;
class Module;

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
  WasmType type = WASM_TYPE_VOID;
  std::string local_name;  // Empty if none bound
};

class Expression {
 public:
  typedef std::vector<std::unique_ptr<Expression>> ExprVector;
  // Common
  WasmOpType opcode = WASM_OP_NOP;
  WasmType expr_type = WASM_TYPE_VOID;
  // Const
  Literal literal = {};
  // Call, CallImport
  int callee_index = 0;
  Callable* callee;
  // Common (block, call args)
  ExprVector exprs;

  Expression(WasmOpType op) : opcode(op) {}
};

class Callable {
 public:
  WasmType result_type = WASM_TYPE_VOID;
  std::vector<Variable> args;
  std::string local_name;  // Empty if none bound
};

class Function : public Callable {
 public:
  std::vector<Variable> locals;
  std::vector<std::unique_ptr<Expression>> body;
  int index_in_module = 0;
  int depth = 0;
};

class Export {
 public:
  Function* function;
  std::string name;
  Module* module;
};

class Import : public Callable {
 public:
  std::string module_name;
  std::string func_name;
};

class Segment {
 public:
  size_t size = 0;
  size_t address = 0;
  std::vector<char> initial_data;
  std::string as_string() const;
};

class Module {
 public:
  std::vector<Segment> segments;
  std::vector<Function> functions;
  std::vector<Export> exports;
  std::vector<Import> imports;
  uint32_t initial_memory_size = 0;
  uint32_t max_memory_size = 0;
  std::string name;
};

}  // namespace wasm
#endif  // WASM_AST_H
